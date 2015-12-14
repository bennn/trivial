#lang racket/base

;; `format:` expressions that should fail to compile

(define (expr->typed-module expr)
  #`(module t typed/racket/base
      (require trivial/format)
      #,expr))

(define TEST-CASE* (map expr->typed-module '(
  (printf: "hello ~a" "john" "doe")
  (printf: "hello ~a" "john" "doe")
  (printf: "binary number ~b\n" 3.14)
  (printf: "character ~c\n" 88)
  (printf: "octl ~o\n" 1.0+2i)
  (printf: "hex ~o\n" (exact->inexact 0))
)))

;; -----------------------------------------------------------------------------

(module+ test
  (require
    rackunit)

  (define (format-eval stx)
    (lambda () ;; For `check-exn`
      (compile-syntax stx)))

  (for ([rkt (in-list TEST-CASE*)])
    (check-exn #rx"format::|Type Checker"
      (format-eval rkt)))
)
