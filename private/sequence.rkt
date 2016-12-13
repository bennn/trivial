#lang racket/base

(provide
  small-sequence-size?
  bounds-error
)

;; =============================================================================

(define (small-sequence-size? n)
  (< n 20))

(define (bounds-error sym v-stx i)
  (raise-syntax-error
    sym
    "Index out-of-bounds"
    (syntax->datum v-stx)
    i
    (list v-stx)))

