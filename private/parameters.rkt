#lang typed/racket/base

(provide
  *TRIVIAL-LOG*
  *STOP-LIST*
  set-trivial-print
)

;; =============================================================================

(: *TRIVIAL-LOG* (Parameterof Boolean))
(define *TRIVIAL-LOG* (make-parameter #f))

(: *STOP-LIST* (Parameterof (Listof Identifier)))
(define *STOP-LIST* (make-parameter '()))

(define (set-trivial-print)
  (*TRIVIAL-LOG* #t)
  (void))
