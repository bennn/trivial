#lang racket/base

(module+ test
  (require
    trivial
    rackunit
    (only-in racket/logging with-intercepted-logging))

  (test-case "double-expand"
    (define num-logs (box 0))
    (with-intercepted-logging
      (lambda (l)
        (when (eq? 'info (vector-ref l 0))
          (set-box! num-logs (+ (unbox num-logs) 1)))
        (void))
      (lambda ()
        (compile-syntax #'(lambda (x) (- (+ x x) 1)))
        (void))
      #:logger trivial-logger
      'info)
    (check-equal? (unbox num-logs) 2))
)
