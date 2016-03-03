#lang racket/base

;; Common helper functions

(provide
  expand-expr
  ;; (-> Syntax Syntax)
  ;; Call local expand for an expression context with an empty list of stop-ids

  quoted-stx-value?
  ;; (-> Any (U #f Syntax))
  ;; If the argument is a syntax object representing a quoted datum `v`,
  ;;  return `v`.
  ;; Otherwise, return #f.

  define-syntax-class/predicate
  ;; (stx-> Identifier (-> Any Boolean) SyntaxClassDef)
)

(require
  syntax/parse
  (for-template (only-in typed/racket/base quote)))

;; =============================================================================

(define-syntax-rule (define-syntax-class/predicate id p?)
  (define-syntax-class id
   #:attributes (expanded)
   (pattern e
    #:with e+ (quoted-stx-value? (expand-expr #'e))
    #:when (p? (syntax-e #'e+))
    #:attr expanded #'e+)))

(define (expand-expr stx)
  (local-expand stx 'expression '()))

(define (quoted-stx-value? stx)
  (and
    (syntax? stx)
    (syntax-case stx (quote)
     [(quote v)
      (syntax-e #'v)]
     [else #f])))
