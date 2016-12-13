#lang racket/base

(provide
  test-compile-error
)

(require
  rackunit
  (for-syntax
    racket/base
    syntax/parse))

;; =============================================================================

(define-syntax (test-compile-error stx)
  (syntax-parse stx
   [(_ #:require r-s* ...
       #:exn exn-rx
       e* ...)
    (syntax/loc stx
      (begin
        (check-exn exn-rx
          (lambda ()
            (compile-syntax #'(module t typed/racket/base (require r-s* ...) e*))))
        ...))]))

