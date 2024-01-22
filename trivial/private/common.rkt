#lang racket/base

;; - Implements proposition environments
;; - Implements 'abstract domains'
;; - Other helpers / parameters

(provide
  ;; --- prop. env.

  φ
  ;; (-> Syntax Phi)
  ;; gets the proposition map associated with its first argument

  ⊢
  ;; (-> Syntax Phi Syntax)
  ;; Attach a proposition map to a syntax object

  φ-tbl
  ;; Global table associating proposition environments to `define`d identifiers
  ;; Very sad.

  φ-mutated
  ;; Global set of set!'d variables.
  ;; Also sad.

  φ?
  ;; (-> any/c boolean?)

  φ-init
  ;; (-> Phi)
  ;; Create an empty proposition map

  φ-ref
  ;; (-> Phi [AbstractDomain X] [Dom X])
  ;; Functionally extend a proposition map with domain-specific data

  φ-set
  ;; (-> Phi [AbstractDomain X] [Dom X] Phi)
  ;; Functionally extend a proposition map with domain-specific data

  φ<=?
  ;; (-> Phi Phi Boolean)
  ;; Precision ordering on φ
  ;; returns #true if first argument is less precise than 2nd on all non-⊥ entries

  ;; --- abstract domains

  ;; type [AbstractDomain X]
  ;;  represents a (flat) lattice of information X

  ;; type [Dom X]
  ;;  (U X ⊤ ⊥)
  ;;  where ⊥ and ⊤ are the bottom & top elements for the abstract domain

  make-abstract-domain
  ;; #(-> Identifier #:leq (-> X X Boolean) [Syntax -> [Dom X]] * [AbstractDomain X])
  ;; Create an 'abstract domain' from:
  ;; - an identifier (symbol)
  ;; - (optional) an order relation
  ;;   Compares pairs of "domain information", lifted to handle ⊥/⊤
  ;;   The default relation returns #false for every pair
  ;; - a sequence of syntax-parse clauses
  ;;   Describe how to parse "domain information" from **values**
  ;;   (Use __TODO__ to parse & propagate information from expressions)

  abstract-domain?

  in-domain?
  ;; (-> abstract-domain? (-> any/c boolean?))

  κ
  ;; (-> AbstractDomain Symbol)
  ;; Return the key for an abstract domain

  ⊥
  ;; (-> AbstractDomain [Dom X])
  ;; Return the bottom element for an abstract domain

  ⊤
  ;; (-> AbstractDomain String [Dom X])
  ;; Convert a domain + error message to a 'top' element

  ⊤-msg
  ;; (-> [Dom X] (U #f String))
  ;; Return the error message from a ⊤ value

  ⊥? ⊤?
  ;; (-> AbstractDomain [Dom X] Boolean)
  ;; Return #true if the argument represents the bot/top element of the given dom.

  ⊓
  ;; (-> AbstractDomain [Dom X] ... [Dom X])
  ;; Greatest lower-bound for a domain

  ⊓*
  ;; (-> AbstractDomain [Listof [Dom X]] [Dom X])

  ⊔
  ;; (-> AbstractDomain [Dom X] ... [Dom X])
  ;; Least upper-bound for a domain

  ⊔*
  ;; (-> AbstractDomain [Listof [Dom X]] [Dom X])

  reduce
  ;; (-> AbstractDomain [X X -> X] X [Dom X] ... [Dom X])

  reduce*
  ;; (-> AbstractDomain [X X -> X] X [Listof [Dom X]] [Dom X])

  make-dmap
  ;; (-> AbstractDomain [X -> Y] AbstractDomain (-> [Dom X] [Dom Y]))
  ;; Lift a function to a ⊥/⊤-preserving map between domains

  ~>
  ;; Syntax class,
  ;; fully expand the argument,
  ;; store the result in the ~> property

  make-lifted-function
  ;; #(-> (-> [Dom X] [Dom X]) [AbstractDomain X] (-> Syntax Syntax))
  ;; Input:
  ;; - F : name of a function (arity 1 only)
  ;; - D : domain of interest
  ;; Returns a 'lifted' version of F
  ;;  that propagates values in D (statically)
  ;;  by applying F at phase 1

  ;; --- utils

  *STOP-LIST*
  ;; (Parameterof (Listof Identifier))
  ;; Sets the stop-list to use during `local-expand`

  ttt-logger
  log-ttt-fatal
  log-ttt-error
  log-ttt-warning
  log-ttt-info
  log-ttt-debug

  log-ttt-infer+
  log-ttt-infer-
  log-ttt-check+
  log-ttt-check-
  ;; (-> Symbol Syntax Log)
  ;; Helpers for logging optimization HITs and MISSes
  ;; - infer+ : got interpolant from program
  ;; - infer- : failed to infer interpolant
  ;; - check+ : transformed, using interpolant
  ;; - check- : tried to transform, but missing interpolant

  ;; --------------------------------------------------------------------------
  ;; -- inlining
  ok-to-unfold?
)

;; -----------------------------------------------------------------------------

(require
  syntax/id-table
  syntax/id-set
  syntax/parse
  (for-syntax racket/base syntax/parse))

;; =============================================================================

(define *STOP-LIST* (make-parameter '()))
(define *abstract-domains* (make-parameter '()))

(define-logger ttt)

(define-syntax-rule (log-ttt-optimization sym stx type)
  (log-ttt-info "[~a:~a:~a] ~a '~a' in '~a'"
                (syntax-source stx)
                (syntax-line stx)
                (syntax-column stx)
                type
                sym
                (syntax->datum stx)))

(define-syntax-rule (log-ttt-infer+ sym stx)
  (log-ttt-optimization sym stx 'INFER+))

(define-syntax-rule (log-ttt-infer- sym stx)
  (log-ttt-optimization sym stx 'INFER-))

(define-syntax-rule (log-ttt-check+ sym stx)
  (log-ttt-optimization sym stx 'CHECK+))

(define-syntax-rule (log-ttt-check- sym stx)
  (log-ttt-optimization sym stx 'CHECK-))

;; =============================================================================

(define φ-key
  (gensym 'φ))

(define φ-tbl
  (make-free-id-table))

(define φ-mutated
  (mutable-free-id-set))

(define (φ stx)
  (or (if (identifier? stx)
        (and (not (free-id-set-member? φ-mutated stx))
             (or (free-id-table-ref φ-tbl stx #f)
                 #;(for/first (((k v) (in-free-id-table φ-tbl))
                             #:when (free-identifier=? stx k))
                     ;; brute-force lookup, see https://github.com/bennn/trivial/issues/52
                     v)
                 (syntax-property stx φ-key)))
        (syntax-property stx φ-key))
      (φ-init)))

(define (⊢ stx new-φ)
  (syntax-property stx φ-key new-φ))

(define (φ? x)
  (and (hash? x)
       (hash-eq? x)))

(define (φ-init)
  (hasheq))

(define (φ-ref φ d)
  (hash-ref φ (κ d) (⊥ d)))

(define (φ-set φ d v)
  (hash-set φ (κ d) v))

(define (φ<=? φ1 φ2)
  (for/and ([(k v) (in-hash φ1)])
    (define D (κ->abstract-domain k))
    (define ⊑/D (abstract-domain-⊑ D))
    (define ⊥/D (abstract-domain-⊥ D))
    (⊑/D v (hash-ref φ2 k ⊥/D))))

;; =============================================================================

(define-syntax (make-abstract-domain stx)
  (syntax-parse stx
   [(_ key
       (~optional (~seq #:leq maybe-leq) #:defaults ([maybe-leq #'#f]))
       clause* ...)
    (quasisyntax/loc stx
      (let* ([bot '#,(gensym (string->symbol (format "~a-⊥" (syntax-e #'key))))]
             [top '#,(gensym (string->symbol (format "~a-⊤" (syntax-e #'key))))]
             [α (syntax-parser clause* ... [_ bot])]
             [leq (or maybe-leq (λ (v1 v2) #f))]
             [⊑ (λ (v1 v2)
                  (or (eq? v1 bot)
                      (and (top/reason? v2) (eq? (top/reason-top v2) top))
                      (if (or (and (top/reason? v1) (eq? (top/reason-top v1) top))
                              (eq? v2 bot))
                        #false
                        (leq v1 v2))))]
             [D (abstract-domain 'key bot top α ⊑)])
        (*abstract-domains* (cons D (*abstract-domains*)))
        D))]))

(struct abstract-domain [
  κ
  ;; key to distinguish this domain in a proposition map

  ⊥ ⊤
  ;; [Dom X]
  ;; distinguished elements

  α
  ;; (-> Syntax [Dom X])
  ;; return the "datum processor" for a Dom
  ;; parses values and returns an element in [Dom X],
  ;;  ideally a value but maybe also an "I don't know" or an "impossible"

  ⊑
  ;; (-> [Dom X] Dom TODO)
])

;; TODO better name
(struct top/reason (
  top ;; Symbol, the top element for a given domain
  msg ;; String, an error message
) #:prefab)

(define (in-domain? d)
  ;; TODO implement me!
  (λ (x) #true))

(define (κ d)
  (abstract-domain-κ d))

(define (κ->abstract-domain k)
  (define D
    (for/first ([d (in-list (*abstract-domains*))]
                #:when (eq? k (abstract-domain-κ d)))
      d))
  (if D D (error 'κ->abstract-domain "invalid key ~a" k)))

(define (⊥ d)
  (abstract-domain-⊥ d))

(define (⊤ d message)
  (define top (abstract-domain-⊤ d))
  (top/reason top message))

(define (⊤-msg v)
  (if (top/reason? v)
    (top/reason-msg v)
    (raise-argument-error '⊤/reason "⊤ element" v)))

(define (⊥? d v)
  (and (symbol? v)
       (eq? (abstract-domain-⊥ d) v)))

(define (⊤? d v)
  (and (top/reason? v)
       (eq? (abstract-domain-⊤ d) (top/reason-top v))))

(define (⊓ d . v*)
  (⊓* d v*))

(define (⊓* d v*)
  (define ⊥/d (abstract-domain-⊥ d))
  (define ⊤/d (⊤ d "glb"))
  (define ⊑/d (abstract-domain-⊑ d))
  (for/fold ([v1 ⊤/d])
            ([v2 (in-list v*)])
    (cond
     [(⊑/d v1 v2) v1]
     [(⊑/d v2 v1) v2]
     [else ⊥/d])))

(define (⊔ d . v*)
  (⊔* d v*))

(define (⊔* d v*)
  (define ⊥/d (abstract-domain-⊥ d))
  (define ⊤/d (⊤ d "lub"))
  (define ⊑/d (abstract-domain-⊑ d))
  (for/fold ([v1 ⊥/d])
            ([v2 (in-list v*)])
    (cond
     [(⊑/d v1 v2) v2]
     [(⊑/d v2 v1) v1]
     [else ⊤/d])))

(define (reduce D f init . v*)
  (reduce* D f init v*))

(define (reduce* D f init v*)
  (if (⊥? D init)
    init
    (let loop ([acc init]
               [v* v*])
      (cond
       [(null? v*)
        acc]
       [else
        (let ([v (car v*)]
              [v* (cdr v*)])
          (if (or (⊥? D v) (⊤? D v))
            v
            (loop (f acc v) v*)))]))))

(define (make-dmap d1 f d2)
  (λ (d1-elem)
    (cond
     [(⊥? d1 d1-elem)
      (⊥ d2)]
     [(⊤? d1 d1-elem)
      (⊤ d2 (⊤-msg d1-elem))]
     [else
      (f d1-elem)])))

;; =============================================================================

(define (expand-expr stx)
  (expand-datum (local-expand stx 'expression (*STOP-LIST*))))

(define (expand-datum stx)
  (syntax-parse stx
   [((~datum quote) v)
    (log-ttt-warning "expanding #%datum ~a" (syntax->datum stx))
    (define φ
      (for/fold ([φ (φ-init)])
                ([d (in-list (*abstract-domains*))])
        (φ-set φ d ((abstract-domain-α d) #'v))))
    (⊢ stx φ)]
   ;; TODO 2016-10-30 : replace variables with their exact value
   [_
    stx]))

(define-syntax-class ~>
  #:attributes (~>)
  (pattern e
   #:attr ~> (expand-expr #'e)))

;; TODO should really declare this as a type, using [Dom X]
(define-syntax-rule (make-lifted-function fn dom)
  (λ (stx)
    (syntax-parse stx
     [(_ e:~>)
      (define φ-e (φ #'e.~>))
      (define v (φ-ref φ-e dom))
      (cond
       [(⊤? dom v)
        (raise-user-error 'fn (⊤-msg v))]
       [else
        (⊢ (syntax/loc stx (fn e.~>))
           (if (⊥? dom v) φ-e (φ-set φ-e dom (fn v))))])]
     [(_ . e*)
      (syntax/loc stx (fn . e*))]
     [_:id
      (syntax/loc stx fn)])))

;; -----------------------------------------------------------------------------

(define-syntax-rule (ttt-log stx msg arg* ...)
  (begin (printf "[LOG:~a:~a] " (syntax-line stx) (syntax-column stx)) (printf msg arg* ...) (newline)))

;; Is `v` a small enough integer to unfold an operation using `v`?
;; e.g., okay to convert `(expt X v)` to `(* X ...v )`
(define (ok-to-unfold? v)
  (<= 0 v 20))

;; =============================================================================

(module+ test
  (require rackunit)

  (define (gen-abstract-domain)
    (let ([k (gensym)]
          [bot (gensym)]
          [top (gensym)])
      (abstract-domain k bot top (λ (stx) bot) (λ (v1 v2) #f))))

  (define flat-N
    (make-abstract-domain flat-N #:leq =
     [i:nat (syntax-e #'i)]))

  (define vert-N
    (make-abstract-domain vert-N #:leq <=
     [i:nat (syntax-e #'i)]))


  (test-case "φ"
    (check-equal? (φ #'#f) (φ-init))
    (check-equal? (φ #'map) (φ-init))
    (check-equal? (φ (syntax-property #'#f φ-key 'val)) 'val))

  (test-case "⊢"
    (let ([stx #'#t]
          [val 'val])
      (check-equal? (φ (syntax-property stx φ-key val)) (φ (⊢ stx val)))))

  (test-case "φ?"
    (check-true (φ? (φ-init)))
    (check-true (φ? (φ-set (φ-init) (gen-abstract-domain) 'v)))

    (check-false (φ? #f))
    (check-false (φ? #'#f))
    (check-false (φ? '())))

  (test-case "φ-ref"
    (let* ([k (gensym)]
           [v (gensym)]
           [d (abstract-domain k 'bot 'top (λ (stx) 'bot) =)])
      (check-equal? (φ-ref (φ-set (φ-init) d v) d) v)))

  (test-case "φ<=?"
    (let* ([φ0 (φ-init)]
           [d1 (gen-abstract-domain)]
           [v1 (gensym 'v)]
           [φ1 (φ-set φ0 d1 v1)]
           [d2 (gen-abstract-domain)]
           [v2 (gensym 'v)]
           [φ2 (φ-set φ1 d2 v2)]
           [d3 (let ([k (gensym)]
                     [bot (gensym)]
                     [top (gensym)])
                 (abstract-domain k bot top (λ (stx) bot) <=))]
           [φ3 (φ-set φ0 d3 10)]
           [φ4 (φ-set φ0 d3 44)])
      (parameterize ([*abstract-domains* (list d1 d2 d3)])
        (check-true (φ<=? φ0 φ0))
        (check-true (φ<=? φ0 φ1))
        (check-true (φ<=? φ0 φ2))
        (check-true (φ<=? φ3 φ4))

        (check-false (φ<=? φ1 φ2)) ;; because ⊑ always returns #f
        (check-false (φ<=? φ1 φ0))
        (check-false (φ<=? φ2 φ3)))))

  (test-case "κ ⊥ ⊤"
    (let* ([key (gensym)]
           [d (abstract-domain key 'bot 'top (λ (stx) 'bot) =)])
      (check-equal? (κ d) key))

    (let* ([bot (gensym)]
           [d (abstract-domain 'key bot 'top (λ (stx) 'bot) =)])
      (check-equal? (⊥ d) bot))

    (let* ([top (gensym)]
           [msg "hello"]
           [d (abstract-domain 'key 'bot top (λ (stx) 'bot) =)])
      (check-equal? (⊤-msg (⊤ d msg)) msg)))

  (test-case "κ->abstract-domain"
    (define (check-failure f)
      (check-exn #rx"invalid key" f))

    (check-failure
      (λ () (κ->abstract-domain 'xyz)))

    (parameterize ([*abstract-domains* (list (gen-abstract-domain))])
      (check-failure
        (λ () (κ->abstract-domain 'xyz))))

    (let ([d (gen-abstract-domain)])
      (parameterize ([*abstract-domains* (list d)])
        (check-equal? (κ->abstract-domain (abstract-domain-κ d)) d))))

  (test-case "⊥? ⊤?"
    (let ([d (gen-abstract-domain)])
      (check-true (⊥? d (⊥ d)))
      (check-true (⊤? d (⊤ d "hey")))

      (check-false (⊥? d (⊤ d "hey")))
      (check-false (⊤? d (⊥ d)))
      (check-false (⊤? d (abstract-domain-⊤ d))) ;; this is intentional
      (check-false (⊥? d #f))
      (check-false (⊤? d #f))))

  (test-case "⊓ ⊔ reduce"
    (check-equal? (⊓ vert-N 1 2 3) 1)
    (check-equal? (⊓ vert-N 1 1) 1)
    (check-equal? (⊓ vert-N 9 8 (⊥ vert-N)) (⊥ vert-N))
    (check-equal? (⊓ vert-N (⊤ vert-N "hi") 4) 4)

    (check-equal? (⊓ flat-N 1 2 3) (⊥ flat-N))
    (check-equal? (⊓ flat-N 1 1) 1)
    (check-equal? (⊓ flat-N (⊥ flat-N) (⊥ flat-N) (⊥ flat-N)) (⊥ flat-N))
    (check-equal? (⊓ flat-N (⊤ flat-N "hi") 4) 4)

    (check-equal? (⊔ vert-N 1 2 3) 3)
    (check-equal? (⊔ vert-N 1 1) 1)
    (check-equal? (⊔ vert-N 9 8 (⊥ vert-N)) 9)
    (check-true (⊤? vert-N (⊔ vert-N (⊤ vert-N "oops"))))

    (check-true (⊤? flat-N (⊔ flat-N 1 2 3)))
    (check-equal? (⊔ flat-N 1 1) 1)
    (check-true (⊤? flat-N (⊔ flat-N (⊤ flat-N "a") 2)))

    (check-equal? (reduce flat-N + 1 2 3) 6)
    (check-equal? (reduce vert-N * 1 2 3 4) 24))

  (test-case "dmap"
    (let ([f (make-dmap flat-N add1 vert-N)]
          [g (make-dmap vert-N add1 flat-N)])
      (check-equal? (f (⊥ flat-N)) (⊥ vert-N))
      (check-equal? (g (⊥ vert-N)) (⊥ flat-N))
      (check-equal? (f 1) 2)
      (check-equal? (g 2) 3)))

  (test-case "expand-datum"
    (check-true (syntax-e (expand-datum #'#t)))
    (check-equal? (syntax-e (expand-datum #'2)) 2)
    (check-equal? (φ (expand-datum #'2)) (φ-init))
    (parameterize ([*abstract-domains* (list flat-N)])
      (check-equal? (φ-ref (φ (expand-datum #''2)) flat-N) 2)))

  (test-case "ok-to-unfold?"
    (check-true (ok-to-unfold? 0))

    (check-false (ok-to-unfold? (expt 10 5))))
)
