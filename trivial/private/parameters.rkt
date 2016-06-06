#lang typed/racket/base

(provide
  *TRIVIAL-LOG*
)

;; =============================================================================

(: *TRIVIAL-LOG* (Parameterof Boolean))
(define *TRIVIAL-LOG* (make-parameter #f))

