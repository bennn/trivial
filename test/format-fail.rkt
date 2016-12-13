#lang racket/base
(require trivial/private/test-common)

;; `format:` expressions that should fail to compile

(module+ test (test-compile-error
  #:require trivial/format trivial/define
  #:exn #rx"format:|printf:|Type Checker"
  (printf "hello ~a" "john" "doe")
  (printf "hello ~a" "john" "doe")
  (printf "binary number ~b\n" 3.14)
  (printf "character ~c\n" 88)
  (printf "octl ~o\n" 1.0+2i)
  (printf "hex ~o\n" (exact->inexact 0))
  (let ()
    (define s "asdf ~a asdf")
    (printf s))
  (let ([s "asdf ~a asdf"])
    (printf s))
))
