#lang racket/base

;; Domains for Racket sequence types

;; TODO
;; - define lifted functions with contracts? to return ⊥ if pre-conditions not met
;; - vector->list homomorphism 'automatically'
;; - ⊓ should be over "lengths", not over I-dom
;; - ⊤ is already covered when these are called. Safe in general???

(provide
  list-domain
  ;; Elements are <φ1 ... φn>
  ;; i.e. finite sequences of proposition maps
  ;; Ordered by `φ*<=?`

  list-domain->I-dom
  I-dom->list-domain
  ;; Composing forgets the contents of φ in the list domain element

  vector-domain
  ;; Elements & order are the same as `list-domain`

  φ*-null?

  vector-domain->I-dom
  I-dom->vector-domain

  vector-domain->list-domain
  list-domain->vector-domain

  (rename-out [I->φ* make-φ*])
  ;; (-> Natural X [Dom X])
  ;; Make a sequence, just like `make-list`

  list-domain-cons
  list-domain-car
  list-domain-first
  list-domain-second
  list-domain-third
  list-domain-cdr
  list-domain-reverse

  list-domain-length
  vector-domain-length

  list-domain-ref
  vector-domain-ref

  list-domain-set
  vector-domain-set

  list-domain-append*
  vector-domain-append*

  list-domain-slice
  vector-domain-slice

  format-bounds-error
  ;; (-> Syntax Integer String)
  ;; (format-bounds-error stx i)
  ;; Make an error message that says `i` is an invalid access to the value
  ;; in the syntax object `stx`.

  format-slice-error
  ;; (-> Syntax Integer Integer String)
  ;; (format-slice-error stx lo hi)
  ;; Make an error message that says `[lo,hi)` is an invalid slice range.

)

(require
  syntax/parse
  trivial/private/common
  (for-meta -1
    (only-in trivial/private/integer I-dom))
  racket/list)

;; =============================================================================

(define (φ*<=? x* y*)
  (or (and (null? x*) (null? y*))
      (and (not (null? x*))
           (not (null? y*))
           (and (φ<=? (car x*) (car y*))
                (φ*<=? (cdr x*) (cdr y*))))))

(define list-domain
  (make-abstract-domain L #:leq φ*<=?
    [(~or '(e* ...) (e* ...))
     (init-φ* (syntax-e #'(e* ...)))]))

(define vector-domain
  (make-abstract-domain V #:leq φ*<=?
    [(~or #(e* ...) '#(e* ...))
     (init-φ* (syntax-e #'(e* ...)))]))

(define (init-φ* e*)
  (parameterize ([*STOP-LIST* #f])
    (for/list ([e (in-list e*)])
      (syntax-parse #`(quote #,e)
       [e:~> (φ #'e.~>)]))))

(define (list-domain->vector-domain l)
  (if (⊥? list-domain l)
    (⊥ vector-domain)
    l))

(define (vector-domain->list-domain v)
  (if (⊥? vector-domain v)
    (⊥ list-domain)
    v))

(define (list-domain-cons φ φ*)
  (if (or (⊥? list-domain φ*) (⊤? list-domain φ*))
    φ*
    (cons φ φ*)))

(define (list-domain-car φ*)
  (if (or (⊥? list-domain φ*) (⊤? list-domain φ*))
    (φ-init)
    (car φ*)))

(define (list-domain-first φ*)
  (list-domain-car φ*))

(define (list-domain-second φ*)
  (if (or (⊥? list-domain φ*) (⊤? list-domain φ*) (< (length φ*) 2))
    (φ-init)
    (second φ*)))

(define (list-domain-third φ*)
  (if (or (⊥? list-domain φ*) (⊤? list-domain φ*) (< (length φ*) 3))
    (φ-init)
    (third φ*)))

(define (list-domain-reverse φ*)
  (if (or (⊥? list-domain φ*) (⊤? list-domain φ*))
    φ*
    (reverse φ*)))

(define (list-domain-cdr φ*)
  (if (or (⊥? list-domain φ*) (⊤? list-domain φ*))
    φ*
    (cdr φ*)))

(define φ*-null?
  null?)

(define φ*?
  list?)

(define (φ*->N φ*)
  (length φ*))

(define φ*-length
  length)

(define φ*-ref
  list-ref)

(define φ*-set
  list-set)

(define (I->φ* n [φ #f])
  (if (⊥? I-dom n)
    (⊥ list-domain)
    (make-list n (or φ (φ-init)))))

(define list-domain->I-dom
  (make-dmap list-domain φ*->N I-dom))

(define I-dom->list-domain
  (make-dmap I-dom I->φ* list-domain))

(define vector-domain->I-dom
  (make-dmap vector-domain φ*->N I-dom))

(define I-dom->vector-domain
  (make-dmap I-dom I->φ* vector-domain))

(define (list-domain-length φ*)
  (cond
   [(⊥? list-domain φ*)
    (⊥ I-dom)]
   [(⊤? list-domain φ*)
    (⊤ I-dom (⊤-msg φ*))]
   [else
    (length φ*)]))

(define (vector-domain-length φ*)
  (cond
   [(⊥? vector-domain φ*)
    (⊥ I-dom)]
   [(⊤? vector-domain φ*)
    (⊤ I-dom (⊤-msg φ*))]
   [else
    (length φ*)]))

(define (lifted-ref φ* i default)
  (if (and (φ*? φ*) (integer? i))
    (φ*-ref φ* i)
    default))

(define (list-domain-ref l i)
  (lifted-ref l i (⊥ list-domain)))

(define (vector-domain-ref v i)
  (lifted-ref v i (⊥ vector-domain)))

(define (lifted-set φ* i x default)
  (if (and (φ*? φ*) (integer? i)) ;; x should really be a φ
    (φ*-set φ* i x)
    default))

(define (list-domain-set l i x)
  (lifted-set l i x (⊥ list-domain)))

(define (vector-domain-set v i x)
  (lifted-set v i x (⊥ vector-domain)))

(define (list-domain-append* φ**)
  (let loop ([x* φ**])
    (cond
     [(null? x*)
      (append* φ**)]
     [(or (⊥? list-domain (car x*)) (⊤? list-domain (car x*)))
      (car x*)]
     [else
      (loop (cdr x*))])))

(define (vector-domain-append* φ**)
  (let loop ([x* φ**])
    (cond
     [(null? x*)
      (append* φ**)]
     [(or (⊥? vector-domain (car x*)) (⊤? vector-domain (car x*)))
      (car x*)]
     [else
      (loop (cdr x*))])))

(define (φ*-slice φ* lo hi)
  (for/list ([φ (in-list φ*)]
             [i (in-naturals)]
             #:when (and (<= lo i) (< i hi)))
    φ))

(define (list-domain-slice φ* lo hi)
  (cond
   [(or (⊥? list-domain φ*) (⊤? list-domain φ*))
    φ*]
   [(or (⊥? I-dom lo) (⊥? I-dom hi))
    (⊥ list-domain)]
   [(⊤? I-dom lo)
    (⊤ list-domain (⊤-msg lo))]
   [(⊤? I-dom hi)
    (⊤ list-domain (⊤-msg hi))]
   [else
    (φ*-slice φ* lo hi)]))

(define (vector-domain-slice φ* lo hi)
  (cond
   [(or (⊥? vector-domain φ*) (⊤? vector-domain φ*))
    φ*]
   [(or (⊥? I-dom lo) (⊥? I-dom hi))
    (⊥ vector-domain)]
   [(⊤? I-dom lo)
    (⊤ vector-domain (⊤-msg lo))]
   [(⊤? I-dom hi)
    (⊤ vector-domain (⊤-msg hi))]
   [else
    (φ*-slice φ* lo hi)]))

(define (format-bounds-error stx i)
  (format "[~a:~a] Index '~a' out of range for '~a'"
          (syntax-line stx)
          (syntax-column stx)
          i
          (syntax->datum stx)))

(define (format-slice-error stx lo hi)
  (format "[~a:~a] Invalid slice range [~a,~a) for '~a'"
          (syntax-line stx)
          (syntax-column stx)
          lo hi
          (syntax->datum stx)))

;; =============================================================================

(module+ test
  (require rackunit)

  (test-case "φ*<=?"
    (let ([φ*1 (list (φ-init))]
          [φ*2 (list (φ-set (φ-init) I-dom 0))]
          [φ*3 (list (φ-set (φ-init) I-dom 1))]
          [φ*4 (for/list ([i (in-range 5)])
                 (φ-set (φ-init) I-dom i))]
          [φ*5 (make-list 5 (φ-set (φ-init) I-dom 5))]
          [φ*6 (list (φ-set (φ-set (φ-init) I-dom 1) vector-domain '()))]
          [φ*7 (list (φ-set (φ-set (φ-init) I-dom 1) vector-domain (list (φ-init))))])
    (check-true (φ*<=? '() '()))
    (check-true (φ*<=? φ*1 φ*1))
    (check-true (φ*<=? φ*1 φ*2))
    (check-true (φ*<=? φ*2 φ*3))
    (check-true (φ*<=? φ*4 φ*5))
    (check-true (φ*<=? φ*3 φ*6))

    (check-false (φ*<=? '() φ*1))
    (check-false (φ*<=? φ*1 '()))
    (check-false (φ*<=? φ*3 φ*2))
    (check-false (φ*<=? φ*5 φ*4))
    (check-false (φ*<=? φ*6 φ*7))
    (check-false (φ*<=? φ*7 φ*1))))

  (test-case "make-φ*"
    (define (check-make-φ* n x)
      (define φ* (I->φ* n x))
      (check-equal? (φ*->N φ*) n)
      (check-equal? φ* (make-list n x)))

    (check-make-φ* 5 'a)
    (check-make-φ* 0 'b)

    (check-equal? (I->φ* (⊥ I-dom) #f) (⊥ list-domain)))

  (test-case "L->N"
    (let ([L->N list-domain->I-dom])
      (check-equal? (L->N '()) 0)
      (check-equal? (L->N (list (φ-init) (φ-init))) 2)
      (check-equal? (L->N (⊥ list-domain)) (⊥ I-dom))))

  (test-case "I->L"
    (let ([I->L I-dom->list-domain])
      (check-equal? (I->L 0) '())
      (check-equal? (I->L 3) (list (φ-init) (φ-init) (φ-init)))
      (check-equal? (I->L (⊥ I-dom)) (⊥ list-domain))))

  (test-case "V->N"
    (let ([V->N vector-domain->I-dom])
      (check-equal? (V->N '()) 0)
      (check-equal? (V->N (make-list 4 (φ-init))) 4)
      (check-equal? (V->N (⊥ vector-domain)) (⊥ I-dom))
      (let* ([msg "dummy"]
             [⊤1 (⊤ vector-domain msg)]
             [⊤2 (⊤ I-dom msg)])
        (check-equal? (⊤-msg (V->N ⊤1)) (⊤-msg ⊤2)))))

  (test-case "I->V"
    (let ([I->V I-dom->vector-domain])
      (check-equal? (I->V 0) '())
      (check-equal? (I->V 3) (list (φ-init) (φ-init) (φ-init)))
      (check-equal? (I->V (⊥ I-dom)) (⊥ vector-domain))
      (let* ([msg "yolo"]
             [⊤1 (I->V (⊤ I-dom msg))]
             [⊤2 (⊤ vector-domain msg)])
        (check-equal?  (⊤-msg ⊤1) (⊤-msg ⊤2)))))

  (test-case "V<=>L"
    (let ([V->L vector-domain->list-domain]
          [L->V list-domain->vector-domain])
      (check-equal? (V->L (⊥ vector-domain)) (⊥ list-domain))
      (check-equal? (L->V (⊥ list-domain)) (⊥ vector-domain))))

  (test-case "cons car cdr"
    ;; TODO
  )

  (test-case "*-domain-length"
    (check-equal? (list-domain-length '(0 0 0)) 3)
    (check-equal? (list-domain-length (⊥ list-domain)) (⊥ I-dom))

    (check-equal? (vector-domain-length '(0 0)) 2)
    (check-equal? (vector-domain-length (⊥ vector-domain)) (⊥ I-dom)))

  (test-case "*-domain-ref"
    (check-equal? (list-domain-ref '(1 2) 0) 1)
    (check-equal? (list-domain-ref (⊥ list-domain) 0) (⊥ list-domain))
    (check-equal? (list-domain-ref '(1) 'other-bot) (⊥ list-domain))
    (check-equal? (list-domain-ref (⊤ list-domain "yo") 2) (⊥ list-domain))

    (check-equal? (vector-domain-ref '(1 2) 0) 1)
    (check-equal? (vector-domain-ref (⊥ vector-domain) 0) (⊥ vector-domain))
    (check-equal? (vector-domain-ref '(1) 'other-bot) (⊥ vector-domain))
    (check-equal? (vector-domain-ref (⊤ vector-domain "yo") 2) (⊥ vector-domain)))

  (test-case "*-domain-set"
    (check-equal? (list-domain-set '(1 2) 0 3) '(3 2))
    (check-equal? (list-domain-set (⊥ list-domain) 0 (φ-init)) (⊥ list-domain))
    (check-equal? (list-domain-set '(1) 'other-bot (φ-init)) (⊥ list-domain))
    (check-equal? (list-domain-set (⊤ list-domain "yo") 2 (φ-init)) (⊥ list-domain))

    (check-equal? (vector-domain-set '(1 2) 0 8) '(8 2))
    (check-equal? (vector-domain-set (⊥ vector-domain) 0 (φ-init)) (⊥ vector-domain))
    (check-equal? (vector-domain-set '(1) 'other-bot (φ-init)) (⊥ vector-domain))
    (check-equal? (vector-domain-set (⊤ vector-domain "yo") 2 (φ-init)) (⊥ vector-domain)))

  (test-case "*-domain-append"
    (check-equal? (list-domain-append* '((1 2))) '(1 2))
    (check-equal? (list-domain-append* '((1) (2) (3))) '(1 2 3))
    (check-equal? (list-domain-append* (list (⊥ list-domain) '(2))) (⊥ list-domain))
    (let ([msg (symbol->string (gensym))])
      (check-equal? (⊤-msg (list-domain-append* (list (⊤ list-domain msg) '()))) msg))

    (check-equal? (vector-domain-append* '((1 2) (3 4) (5))) '(1 2 3 4 5))
    (check-equal? (vector-domain-append* (list (⊥ vector-domain) '())) (⊥ vector-domain))
    (let ([msg "secret"])
      (check-equal? (⊤-msg (vector-domain-append* (list (⊤ vector-domain msg) '() '(3 3)))) msg)))

  (test-case "*-domain-slice"
    (define (check-slice slice-op dom)
      (check-equal? (slice-op '(1 2) 0 0) '())
      (check-equal? (slice-op '(1 2) 0 1) '(1))
      (check-equal? (slice-op '(1 2) 0 2) '(1 2))
      (check-equal? (slice-op '(1 2) 1 2) '(2))
      (check-equal? (slice-op (⊥ dom) 0 0) (⊥ dom))
      (check-equal? (slice-op '(1) (⊥ I-dom) 1) (⊥ dom))
      (check-equal? (slice-op '(1) 1 (⊥ I-dom)) (⊥ dom))
      (let ([msg "msg"])
        (check-equal? (⊤-msg (slice-op (⊤ dom msg) 2 3)) msg)
        (check-equal? (⊤-msg (slice-op '() (⊤ I-dom msg) 3)) msg)
        (check-equal? (⊤-msg (slice-op '() 2 (⊤ I-dom msg))) msg)))

    (check-slice list-domain-slice list-domain)
    (check-slice vector-domain-slice vector-domain))

  (test-case "format-bounds-error"
    (define (check-bounds-error stx i)
      (define msg (format-bounds-error stx i))
      (check-regexp-match ;; has line/column
        (format "\\[~a:~a\\]" (syntax-line stx) (syntax-column stx))
        msg)
      (check-regexp-match ;; has index
        (format " '~a' " i)
        msg)
      (check-regexp-match ;; says what's wrong
        "out of range"
        msg)
      (void)) ;; don't really care about syntax object

    (check-bounds-error #''(foo bar) 3)
    (check-bounds-error #'#f 8)
    (check-bounds-error #'4 4))

)
