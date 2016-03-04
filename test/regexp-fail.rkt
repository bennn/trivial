#lang racket/base
(require
  trivial/private/test-common
  (only-in typed/racket/base
    ann : -> String Listof List U Bytes))

;; Ill-typed `regexp:` expressions
;; TODO why can't I catch errors for (ann ... (List String))? WhydoI need #f?


(module+ test (test-compile-error
  #:require trivial/regexp
  #:exn #rx"Type Checker"
  (ann (regexp-match: "hi" "hi")
       (U #f (List String String String)))
  (ann (regexp-match: #rx"(h)(i)" "hi")
       (U #f (List String)))
  (ann (regexp-match: #px"(?<=h)(?=i)" "hi")
       (U #f (List String String String)))
  ;;bg; ill-typed in untyped Racket
  (byte-regexp: #rx#"yolo")
  (ann (regexp-match: #rx#"hi" "hi")
       (U #f (List String String)))
  (ann (regexp-match: #px#"hi" "hi")
       (U #f (List Bytes Bytes)))
  (ann (regexp-match: (regexp "he") "hellooo")
       (U #f (List String)))
  (ann (let ()
         (define-regexp: rx (regexp "he(l*)(o*)"))
         (regexp-match: rx "hellooo"))
       (U #f (List String String String)))
  ;; `define` doesn't propagate group information
  (ann (let ()
         (define rx "he(l*)(o*)")
         (regexp-match: rx "helloooooooo"))
       (U #f (List String String String)))
  ;; -- set! problems
  (ann (let-regexp: ([a #rx"(b)(B)"])
         (set! a #rx"")
         (regexp-match: a "hai"))
       (List String String String))
  ;; --- Can't handle |, yet
  (ann (regexp-match: "this(group)|that" "that")
       (U #f (List String String)))
))
