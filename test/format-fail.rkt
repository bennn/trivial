#lang racket/base

;; `format:` expressions that should fail to compile

;; TODO abstract the 'module ...'
(define TEST-CASE* (syntax->list #'(
  (module t typed/racket/base (require trivial/format)
    (printf: "hello ~a" "john" "doe"))
  (module t typed/racket/base (require trivial/format)
    (printf: "hello ~a" "john" "doe"))
  (module t typed/racket/base (require trivial/format)
    (printf: "binary number ~b\n" 3.14))
  (module t typed/racket/base (require trivial/format)
    (printf: "character ~c\n" 88))
  (module t typed/racket/base (require trivial/format)
    (printf: "octl ~o\n" 1.0+2i))
  (module t typed/racket/base (require trivial/format)
    (printf: "hex ~o\n" (exact->inexact 0)))
)))

(module+ test
  (require
    rackunit)

  (define format-eval
    (let ([format-ns (make-base-namespace)])
      (lambda (stx)
        (lambda () ;; For `check-exn`
          (eval-syntax stx format-ns)))))

  (for ([rkt (in-list TEST-CASE*)])
    (check-exn #rx"format::|Type Checker"
      (format-eval rkt)))
)
