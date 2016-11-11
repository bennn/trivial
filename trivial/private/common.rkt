#lang racket/base

;; - Implements proposition environments
;; - Implements 'abstract domains'
;; - Other helpers / parameters

(provide
  ;; --- prop. env.

  φ
  ;; (-> Syntax Phi)
  ;; gets the proposition map associated with its first argument
  ;; When the 2nd argument is non-#f, returns the corresponding
  ;;  value for the domain instead of the map.

  ⊢
  ;; (-> Syntax Phi Syntax)
  ;; Attach a proposition map to a syntax object

  φ-tbl
  ;; Global table associating proposition environments to `define`d identifiers
  ;; Very sad.

  φ-mutated
  ;; Global set of set!'d variables.
  ;; Also sad.

  φ-init
  ;; (-> Phi)
  ;; Create an empty proposition map

  φ-ref
  ;; (-> Phi [AbstractDomain X] [Dom X])
  ;; Functionally extend a proposition map with domain-specific data

  φ-set
  ;; (-> Phi [AbstractDomain X] [Dom X] Phi)
  ;; Functionally extend a proposition map with domain-specific data

  ;; --- abstract domains

  ;; type [AbstractDomain X]
  ;;  represents a (flat) lattice of information X
  ;;  TODO probably needs a 'less than' and 'join' operation

  ;; type [Dom X]
  ;;  (U X ⊤ ⊥)
  ;;  where ⊥ and ⊤ are the bottom & top elements for the abstract domain

  make-abstract-domain
  ;; #(-> Identifier [Syntax -> [Dom X]] * [AbstractDomain X])
  ;; Create an 'abstract domain' from an identifier (symbol)
  ;;  and a sequence of syntax-parse clauses
  ;; The clauses describe how to parse domain information from **values**
  ;; (To parse information from expressions, attach it using `⊢` from `phi.rkt`)

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
  ;; TODO hack for now, need to put an order on domains

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
                 (syntax-property stx φ-key)))
        (syntax-property stx φ-key))
      (φ-init)))

(define (⊢ stx new-φ)
  (syntax-property stx φ-key new-φ))

(define (φ? x)
  (hash-eq? x))

(define (φ-init)
  (hasheq))

(define (φ-ref φ d)
  (hash-ref φ (κ d) (⊥ d)))

(define (φ-set φ d v)
  (hash-set φ (κ d) v))

;; =============================================================================

(define-syntax (make-abstract-domain stx)
  (syntax-parse stx
   [(_ key clause* ...)
    #:with bot (gensym (string->symbol (format "~a-⊥" (syntax-e #'key))))
    #:with top (gensym (string->symbol (format "~a-⊤" (syntax-e #'key))))
    #'(let ([d (abstract-domain 'key 'bot 'top (syntax-parser clause* ... [_ 'bot]))])
        (*abstract-domains* (cons d (*abstract-domains*)))
        d)]))

(struct abstract-domain [
  κ
  ;; key to distinguish this domain in a proposition map

  ⊥ ⊤
  ;; [Dom X]
  ;; distinguished elements

  ⊣
  ;; (-> Syntax [Dom X])
  ;; return the "datum processor" for a Dom
  ;; parses values and returns an element in [Dom X],
  ;;  ideally a value but maybe also an "I don't know" or an "impossible"
])

;; TODO shitty name
(struct top/reason (
  top ;; Symbol, the top element for a given domain
  msg ;; String, an error message
))

(define (κ d)
  (abstract-domain-κ d))

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

(define (⊓ d v*)
  (define ⊥/d (⊥ d))
  (define ⊤/d (⊤ d "glb"))
  (define (</d v1 v2) ;; TODO this should be part of d
    (cond
     [(⊤? d v1)                v2]
     [(⊤? d v2)                v1]
     [(or (⊥? d v1) (⊥? d v2)) ⊥/d]
     [(< v1 v2)                v1]
     [else                     v2]))
  (for/fold ([glb ⊤/d])
            ([v (in-list v*)])
    (</d glb v)))

;; TODO ⊔

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
        (φ-set φ d ((abstract-domain-⊣ d) #'v))))
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

(module+ test ;; TODO
)
