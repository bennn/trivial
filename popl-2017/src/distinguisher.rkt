#lang racket/base
(require (for-syntax racket/base syntax/parse))

(define-syntax (distinguisher stx)
  (syntax-parse stx
   [(_ e_1 e_2)
    (if (equal? (syntax->datum #'e_1)
                (syntax->datum #'e_2))
      #'((λ (x) (x x)) (λ (x) (x x)))
      #'(λ (x) x))]))

;(distinguisher '() '())
(distinguisher 'A 'B)
