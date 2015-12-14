#lang racket/base

;; Common helper functions
;; TODO actually detect the `quote` identifier, don't use eq?

(provide
  expand-expr
  ;; (-> Syntax Syntax)
  ;; Call local expand for an expression context with an empty list of stop-ids

  quoted-stx-value?
  ;; (-> Any (U #f Syntax))
  ;; If the argument is a syntax object representing a quoted #%datum `v`,
  ;;  return `v`.
  ;; Otherwise, return #f.
)

(require
  (for-template (only-in racket/base quote)))

;; =============================================================================

(define (expand-expr stx)
  (local-expand stx 'expression '()))

(define (quoted-stx-value? stx)
  (and
    (syntax? stx)
    (syntax-case stx (quote)
     [(quote v)
      (syntax-e #'v)]
     [else #f])))
