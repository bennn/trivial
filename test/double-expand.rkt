#lang racket/base

;; Each identifier from `trivial` should produce exactly 1 log message

(module+ test
  (require
    (only-in trivial lambda + -)
    rackunit
    trivial/private/test-common)

  (test-case "double-expand"

    (check-trivial-log-sequence
      (lambda (x) (- (+ x x) 1))
      '((INFER+ lambda)
        (CHECK- +)
        (CHECK- -))))
)
