#lang racket/base

(provide
  expand-expr
  ;; (-> Syntax Syntax)
  ;; Call local expand for an expression context with an empty list of stop-ids
)

;; =============================================================================

(define (expand-expr stx)
  (local-expand stx 'expression '()))
