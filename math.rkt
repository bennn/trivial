#lang typed/racket/base

(provide
  +: ;-: *: /:
  ;; Fold syntactic constants
)

(require (for-syntax
  racket/base
  (only-in racket/format ~a)
  racket/syntax
  syntax/id-table
  syntax/parse
  trivial/private/common
))

;; =============================================================================

(define-syntax make-numeric-operator
  (syntax-parser
   [(_ f:id)
    #:with f: (format-id #'f "~a:" (syntax-e #'f))
    #'(define-syntax f:
        (syntax-parser
         [g:id
          (syntax/loc #'g f)]
         [(g e* (... ...))
          #:with e+* (for/list ([e (in-list (syntax->list #'(e* (... ...))))])
                       (expand-expr e))
          #:with e++ (reduce/op f (syntax->list #'e+*) #:src #'g)
          (syntax/loc #'g e++)]
         [(g e* (... ...))
          (syntax/loc #'g (f e* (... ...)))]))]))

(make-numeric-operator +)

;; -----------------------------------------------------------------------------

(define-for-syntax (reduce/op op e* #:src stx)
  (let loop ([prev #f]
             [acc  '()]
             [e*   e*])
    (if (null? e*)
      ;; then: combine `prev` and `acc` into a list or single number
      (cond
       [(null? acc)
        (quasisyntax/loc stx #,prev)]
       [else
        (let ([acc+ (reverse (if prev (cons prev acc) acc))])
          (quasisyntax/loc stx (#,op #,@acc+)))])
      ;; else: pop the next argument from e*, fold if it's a constant
      (syntax-parse (car e*)
       [n:number
        (if prev
          ;; eval?
          (loop (op prev (car e*)) acc (cdr e*))
          (loop (car e*) acc (cdr e*)))]
       [e
        (loop #f (cons (car e*) (if prev (cons prev acc) acc)) (cdr e*))]))))
