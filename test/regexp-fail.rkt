#lang racket/base
(require
  trivial/private/test-common
  (only-in typed/racket/base
    ann : -> String Listof List U Bytes))

;; Ill-typed `regexp:` expressions
;; TODO why can't I catch errors for (ann ... (List String))? WhydoI need #f?

(module+ test (test-compile-error
  #:require trivial/regexp trivial/define
  #:exn #rx"Type Checker"
  (ann (regexp-match "hi" "hi")
       (U #f (List String String String)))
  (ann (regexp-match #rx"(h)(i)" "hi")
       (U #f (List String String)))
  (ann (regexp-match #px"(?<=h)(?=i)" "hi")
       (U #f (List String String String)))
  ;;bg; ill-typed in untyped Racket
  (byte-regexp #rx#"yolo")
  (ann (regexp-match #rx#"hi" "hi")
       (U #f (List String String)))
  (ann (regexp-match #px#"hi" "hi")
       (U #f (List Bytes Bytes)))
  ;; --- Can't handle |, yet
  (ann (regexp-match "this(group)|that" "that")
       (U #f (List String String)))
  ;; --- can't handle starred groups
  (ann (regexp-match "(a)*(b)" "b")
       (U #f (List String String String)))
)

(test-compile-error
  #:require trivial/regexp racket/port trivial/define
  #:exn #rx"Type Checker"
  ;; -- expected String, given Bytes
  (with-input-from-string "hello"
    (lambda ()
      (define m (regexp-match #rx#"lang" (current-input-port)))
      (and m (string=? (car m) "lang"))))

  ;; ---- is raising a type error, which is GOOD, but throwing during test
  ;; -- return type assumed to be String, but really is Bytes
  ;;    (ugly, but at least we catch it statically)
  ;(with-input-from-file "test/regexp-fail.rkt"
  ;  (lambda ()
  ;    (define m (regexp-match #rx"lang" (current-input-port)))
  ;    (and m (string=? (car m) #"lang"))))
)

;; 2016-06-13 : these really should be errors, just no-opts
;(test-compile-error
;  #:require trivial/regexp trivial/define
;  #:exn #rx"mutation not allowed"
;  ;; -- set! problems
;  (ann (let ([a #rx"(b)(B)"])
;         (set! a #rx"")
;         (regexp-match a "hai"))
;       (List String String String))
;  (let ()
;    (define a #rx"h(i)")
;    (set! a #rx"hi")
;    (regexp-match a "hi"))
;
;  (let ([a #rx"h(i)"])
;    (set! a #rx"(h)(i)")
;    (regexp-match a "hi"))
;)
)
