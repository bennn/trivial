#lang typed/racket/base

(require trivial/regexp)

;(: m1 (U #f (List String)))
;(define m1 (regexp-match: "hello" "hello world"))
;
;(: m2 (U #f (List String)))
;(define m2 (regexp-match: "hello" "world"))
;
;(: m3 (U #f (List String String)))
;(define m3 (regexp-match: "he(l*)o" "hellllloooo"))
;
;(: m4 (U #f (List String String String)))
;(define m4 (regexp-match: "he(l*)(o*)" "hellllloooo"))

(: m5 (U #f (List String String String)))
(define m5
  (let ()
    (define-regexp: rx "he(l*)(o*)")
    (regexp-match: rx "helloooooooo")))

;(: m6 (U #f (List String String String)))
;(define m6
;  (let ()
;    (define-regexp: rx #rx"he(l*)(o*)")
;    (regexp-match: rx "helloooooooo")))

;(: m7 (U #f (List String String String)))
;(define m7
;  (let ()
;    (define-regexp: rx (regexp "he(l*)(o*)"))
;    (regexp-match: rx "helloooooooo")))

;(: m6 (U #f (List String String)))
;(define m5 (regexp-match: #rx"he(l*)o" "hellllloooo"))

