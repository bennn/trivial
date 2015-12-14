#lang racket/base

;; Math expressions that fail to typecheck

(define (expr->typed-module expr)
  #`(module t typed/racket/base
      (require trivial/math)
      #,expr))

(define TEST-CASE* (map expr->typed-module '(
  (ann (let ([n 2]) (+: n -2)) Zero)
  (ann (let ([n 2]) (-: 2 n)) Zero)
  (ann (let ([n 5]) (*: n 1/5 1)) One)
  (ann (let ([n 4]) (/: n n)) One)
  ;; -- lambda => back to racket/base
  (ann ((lambda ([f : (-> Natural Natural Natural)]) (f 0 0)) +:) Zero)
  (ann ((lambda ([f : (-> Natural Natural Integer)]) (f 0 0)) -:) Zero)
  (ann ((lambda ([f : (-> Natural Natural Natural)]) (f 0 0)) *:) Zero)
  (ann ((lambda ([f : (-> Natural Natural Exact-Rational)]) (f 0 0)) /:) Zero)
  ;; -- dividing by zero => fall back to racket/base
  (ann (/: 1 1 0) One)
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
