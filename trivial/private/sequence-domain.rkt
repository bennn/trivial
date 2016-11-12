#lang racket/base

(provide
  L-dom
  V-dom
  format-bounds-error)

(require
  syntax/parse
  trivial/private/common)

;; =============================================================================

(define L-dom
  (make-abstract-domain L #:leq <=
    [(~or '(e* ...)
           (e* ...))
     (length (syntax-e #'(e* ...)))]))

(define V-dom
  (make-abstract-domain V #:leq <=
    [(~or #(e* ...) '#(e* ...))
     (length (syntax-e #'(e* ...)))]))

(define (format-bounds-error stx i)
  (format "[~a:~a] Index '~a' out of range for '~a'"
          (syntax-line stx)
          (syntax-column stx)
          i
          (syntax->datum stx)))

;; =============================================================================

(module+ test
)
