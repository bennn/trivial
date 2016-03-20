#lang typed/racket/base

(provide
  (rename-out [set!: set!])
)

(require
  (for-syntax
    racket/base
    syntax/parse
    trivial/private/common)
  (only-in trivial/private/db connection-key)
  (only-in trivial/private/math num-key)
  (only-in trivial/private/regexp rx-key)
  (only-in trivial/private/vector vector-length-key)
)

;; =============================================================================

(define-for-syntax (has-important-syntax-property? stx)
  (or #t)) ;; Safe over-approximation
;   (syntax-property stx connection-key)
;   (syntax-property stx num-key)
;   (syntax-property stx rx-key)
;   (syntax-property stx vector-length-key)))

(define-syntax set!: (make-keyword-alias 'set!
  (lambda (stx) (syntax-parse stx
   [(_ name val)
    #:when (has-important-syntax-property? #'name)
    (raise-syntax-error 'trivial "mutation not allowed"); stx); #'name)
    #'(void)]
   [_ #f]))))
