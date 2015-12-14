#lang typed/racket/base

(provide
  +: -: *: /:
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
         [(g e* (... ...))
          #:with e+* (for/list ([e (in-list (syntax->list #'(e* (... ...))))])
                       (expand-expr e))
          (let ([e++ (reduce/op f (syntax->list #'e+*))])
            (if (list? e++)
              (quasisyntax/loc #'g (f #,@e++))
              (quasisyntax/loc #'g #,e++)))]
         [g:id
          (syntax/loc #'g f)]
         [(g e* (... ...))
          (syntax/loc #'g (f e* (... ...)))]))]))

(make-numeric-operator +)
(make-numeric-operator -)
(make-numeric-operator *)
(make-numeric-operator /)

;; -----------------------------------------------------------------------------

(define-for-syntax (reduce/op op e*)
  (let loop ([prev #f]
             [acc  '()]
             [e*   e*])
    (if (null? e*)
      ;; then: finished, return a number (prev) or list of expressions (acc)
      (if (null? acc)
        prev
        (reverse (if prev (cons prev acc) acc)))
      ;; else: pop the next argument from e*, fold if it's a constant
      (let ([v (quoted-stx-value? (car e*))])
        (if (number? v)
          ;; then: reduce the number
          (if prev
            ;; Watch for division-by-zero
            (if (and (zero? v) (eq? / op))
              (loop v (cons prev acc) (cdr e*))
              (loop (op prev v) acc (cdr e*)))
            (loop v acc (cdr e*)))
          ;; else: save value in acc
          (let ([acc+ (cons (car e*) (if prev (cons prev acc) acc))])
            (loop #f acc+ (cdr e*))))) )))
