#lang racket/base

;; Integer arithmetic + contant propagation

(provide
  (for-syntax I-dom)
  (rename-out
   [-+ +]
   [-- -]
   [-* *]
   [-/ /]
   [-expt expt]
   [-quotient quotient])
)

;; -----------------------------------------------------------------------------

(require
  syntax/parse
  (for-syntax
    syntax/parse
    racket/syntax
    racket/base
    trivial/private/common))

;; =============================================================================

(define-for-syntax I-dom
  (make-abstract-domain I
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
            (let ([acc+ (cons (car e*) (if prev (cons prev acc) acc))])
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
            (if (list? e+)
              (quasisyntax/loc stx (#%app f-id #,@e+))
              (⊢ (quasisyntax/loc stx #,e+)
                 (φ-set (φ-init) I-dom e+))))]
         [_:id #'f]))]))

(make-numeric-operator +)
(make-numeric-operator -)
(make-numeric-operator *)
(make-numeric-operator /)

(define-syntax (-expt stx)
  (syntax-parse stx
   [(_ n1:~> n2:~>)
    (define n1-val (φ-ref (φ #'n1.~>) I-dom))
    (define n2-val (φ-ref (φ #'n2.~>) I-dom))
    (cond
     [(and (integer? n1-val) (integer? n2-val))
      (define n1n2 (expt n1-val n2-val))
      (⊢ (quasisyntax/loc stx '#,n1n2)
         (φ-set (φ-init) I-dom '#,n1n2))]
     [(and (integer? n2-val) (ok-to-unfold? n2-val))
      (if (zero? n2-val)
        (⊢ (quasisyntax/loc stx 1)
           (φ-set (φ-init) I-dom 1))
        (quasisyntax/loc stx
          (* #,@(for/list ([_i (in-range n2-val)]) (quasisyntax/loc stx n1)))))]
     [else
      #'(expt n1.~> n2.~>)])]
   [(_ . n*)
    #'(expt . n*)]
   [_:id #'expt]))

(define-syntax (-quotient stx)
  (syntax-parse stx
   [(_ n1:~> n2:~>)
    (define n1-val (φ-ref (φ #'n1.~>) I-dom))
    (define n2-val (φ-ref (φ #'n2.~>) I-dom))
    (cond
     [(and (integer? n1-val) (integer? n2-val))
      (define n1/n2 (quotient n1-val n2-val))
      (⊢ (quasisyntax/loc stx '#,n1/n2)
         (φ-set (φ-init) I-dom '#,n1/n2))]
     [else
      #'(quotient n1.~> n2.~>)])]
   [(_ . n*)
    #'(quotient . n*)]
   [_:id #'quotient]))
