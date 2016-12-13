#lang racket/base

;; Integer arithmetic + contant propagation

(provide
  (for-syntax I-dom)
  (rename-out
   [-+ +]
   [-- -]
   [-* *]
   [-/ /]
   [-add1 add1]
   [-sub1 sub1]
   [-expt expt]
   [-quotient quotient])
)

;; -----------------------------------------------------------------------------

(require
  syntax/parse
  trivial/private/tailoring
  (prefix-in τ- (only-in typed/racket/base * add1 sub1 expt quotient))
  (prefix-in λ- (only-in racket/base * add1 sub1 expt quotient))
  (for-syntax
    syntax/parse
    racket/syntax
    racket/base
    trivial/private/common))

;; =============================================================================

(define-for-syntax I-dom
  (make-abstract-domain I #:leq <=
   [i:integer
    (syntax-e #'i)]))

(define-for-syntax (division-by-zero stx)
  (raise-syntax-error '/ "division by zero" stx))

;; Simplify a list of expressions using an associative binary operator.
;; Return either:
;; - A numeric value
;; - A list of syntax objects, to be spliced back in the source code
(define-for-syntax (reduce/op op stx)
  (define expr* (syntax-e stx))
  (cond
   [(list? expr*)
    (let loop ([prev #f]      ;; (U #f Number), candidate for reduction
               [acc  '()]     ;; (Listof Syntax), irreducible arguments
               [e*   expr*])  ;; (Listof Syntax), arguments to process
      (if (null? e*)
        ;; then: finished, return a number (prev) or list of expressions (acc)
        (if (null? acc)
          prev
          (reverse (if prev (cons prev acc) acc)))
        ;; else: pop the next argument from e*, fold if it's a constant
        (syntax-parse (car e*)
         [e+:~>
          (define v (φ-ref (φ #'e+.~>) I-dom))
          (if (integer? v)
            ;; then: reduce the number
            (if prev
              ;; Watch for division-by-zero
              (if (and (zero? v) (eq? / op))
                (division-by-zero stx)
                (loop (op prev v) acc (cdr e*)))
              (loop v acc (cdr e*)))
            ;; else: save value in acc
            (let ([acc+ (cons #'e+.~> (if prev (cons prev acc) acc))])
              (loop #f acc+ (cdr e*))))])))]
   [else  #f]))

(define-syntax make-numeric-operator
  (syntax-parser
   [(_ f:id)
    #:with -f (format-id #'f "-~a" (syntax-e #'f))
    #'(define-syntax (-f stx)
        (syntax-parse stx
         [(_ e* (... ...))
          #:with f-id (format-id stx "~a" 'f)
          (let ([e+ (reduce/op f #'(e* (... ...)))])
            (cond
             [(list? e+)
              (log-ttt-check- 'f stx)
              (quasisyntax/loc stx (#%app f-id #,@e+))]
             [else
              (log-ttt-check+ 'f stx)
              (⊢ (quasisyntax/loc stx #,e+)
                 (φ-set (φ-init) I-dom e+))]))]
         [_:id #'f]))]))

(make-numeric-operator +)
(make-numeric-operator -)
(make-numeric-operator *)
(make-numeric-operator /)

(define-tailoring (-add1 [e ~> e+ (φ [I-dom ↦ i])])
  #:with +add1 (τλ #'τ-add1 #'λ-add1)
  #:= (⊥? I-dom i)
      (+add1 e+)
  #:+ #t
      '#,(+ i 1)
  #:φ (φ-set (φ-init) I-dom (reduce I-dom + i 1)))

(define-tailoring (-sub1 [e ~> e+ (φ [I-dom ↦ i])])
  #:with +sub1 (τλ #'τ-sub1 #'λ-sub1)
  #:= (⊥? I-dom i)
      (+sub1 e+)
  #:+ #t
      '#,(- i 1)
  #:φ (φ-set (φ-init) I-dom (reduce I-dom - i 1)))

(define-tailoring (-expt [e1 ~> e1+ (φ1 [I-dom ↦ i1])]
                         [e2 ~> e2+ (φ2 [I-dom ↦ i2])])
  #:with +expt (τλ #'τ-expt #'λ-expt)
  #:with +* (τλ #'τ-* #'λ-*)
  (define new-i
    (let ([i1-⊥? (⊥? I-dom i1)]
          [i2-⊥? (⊥? I-dom i2)])
      (cond
       [(and (not i1-⊥?) (zero? i1))
        0]
       [(and (not i2-⊥?) (zero? i2))
        1]
       [(and (not i1-⊥?) (not i2-⊥?))
        (expt i1 i2)]
       [else
        (⊥ I-dom)])))
  (define success?
    (or (not (⊥? I-dom new-i))
        (and (not (⊥? I-dom i2)) (ok-to-unfold? i2))))
  #:= (not success?)
      (+expt e1+ e2+)
  #:+ #t
      #,(if (⊥? I-dom new-i)
          #`(+* #,@(for/list ([_i (in-range i2)]) #'e1+))
          new-i)
  #:φ (φ-set (φ-init) I-dom new-i))

(define-tailoring (-quotient [e1 ~> e1+ (φ1 [I-dom i1])]
                             [e2 ~> e2+ (φ2 [I-dom i2])])
  #:with +quotient (τλ #'τ-quotient #'λ-quotient)
  #:= (or (⊥? I-dom i1) (⊥? I-dom i2))
      (+quotient e1+ e2+)
  #:+ #t
      '#,(quotient i1 i2)
  #:φ (φ-set (φ-init) I-dom (reduce I-dom quotient i1 i2)))

