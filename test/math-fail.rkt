#lang racket/base
(require
  trivial/private/test-common
  (only-in typed/racket/base ann lambda One Zero -> : Natural Exact-Rational))

;; Math expressions that fail to typecheck


(module+ test (test-compile-error
  #:require trivial/math
  #:exn #rx"quotient:|/:|Type Checker"
  (ann (let ([n 2]) (+: n -2)) Zero)
  (ann (let ([n 2]) (-: 2 n)) Zero)
  (ann (let ([n 5]) (*: n 1/5 1)) One)
  (ann (let ([n 4]) (/: n n)) One)
  (ann (let ([n 2]) (expt: 3 (-: n n))) One)
  (ann (expt: 3 2) Zero)
  (ann (quotient: 3 3) Zero)
  (ann ((lambda ([x : Natural]) (expt x 3)) 2) Index)
  ;; -- lambda => back to racket/base
  (ann ((lambda ([f : (-> Natural Natural Natural)]) (f 0 0)) +:) Zero)
  (ann ((lambda ([f : (-> Natural Natural Integer)]) (f 0 0)) -:) Zero)
  (ann ((lambda ([f : (-> Natural Natural Natural)]) (f 0 0)) *:) Zero)
  (ann ((lambda ([f : (-> Natural Natural Exact-Rational)]) (f 0 0)) /:) Zero)
  (ann ((lambda ([f : (-> Natural Natural Natural)]) (f 0 1)) expt:) Zero)
  ;; -- dividing by zero => caught statically
  (/: 1 1 0)
  (/: 1 1 (+: 4 -2 -2))
  (quotient: 9 0)
  ;; -- redefine ops => fail
  (ann (let ([+: (lambda (x y) "hello")]) (+: 1 1)) Integer)
  (ann (let ([-: (lambda (x y) "hello")]) (-: 1 1)) Integer)
  (ann (let ([/: (lambda (x y) "hello")]) (/: 1 1)) Integer)
  (ann (let ([*: (lambda (x y) "hello")]) (*: 1 1)) Integer)
  (ann (let ([expt: (lambda (x y) "hello")]) (expt: 1 1)) Integer)
))
