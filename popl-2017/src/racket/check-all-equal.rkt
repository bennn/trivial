#lang racket/base

(module+ test
  (require rackunit)

  (define-syntax-rule (check-all-eq? e_1 e_rest ...)
    (begin (check-equal? e_1 e_1)
           (check-equal? e_rest e_1) ...))

  (check-all-eq? 1 1 1 1 1)
  (check-all-eq? 1 2 1 3)
)
