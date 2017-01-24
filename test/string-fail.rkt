#lang racket/base
(require
  trivial/private/test-common)

(module+ test (test-compile-error
  #:require trivial/string
  #:exn #rx"out of range"
  (string-ref "" 0)
  (string-ref "abc" 3)
  (string-ref "a" -1)
  ;;(string-set! "" 0 #\x)
  ;;(string-set! "a" 3 ((lambda (x) x) #\A)) ;; TODO not a compile-time error
  (substring "asdf" -3)
  (substring "asdf" 0 8)
  (substring "asdf" 0 -1)
  (substring "asdf" 99 3)

  (bytes-ref #"" 8)
  (bytes-ref #"aaa" -1)
  ;;(bytes-set! #"a" 1 3)
  ;;(bytes-set! #"a" -1 69)
  (subbytes #"xxx" -2)
  (subbytes #"xxx" 2 4)
  (subbytes #"xxx" -8 -2)))

(module+ test (test-compile-error
  #:require trivial/string
  #:exn #rx"Invalid slice range"
  (substring "asdf" 2 1)
  (subbytes #"asdf" 1 0)))

