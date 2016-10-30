#lang racket/base

;; Implement GENERIC elaboration rules,
;;  propagating prop. environments φ

(provide
  (rename-out
    [-let let]
    [-let* let*]
    [-define define]))

;; -----------------------------------------------------------------------------

(require
  (prefix-in tr- (only-in typed/racket/base define let))
  (for-syntax
    trivial/private/common
    typed/untyped-utils
    racket/base
    racket/syntax
    syntax/parse
    syntax/id-table))

;; =============================================================================

(define-syntax (--let stx)
  (with-syntax ([+let (if (syntax-local-typed-context?) (syntax/loc stx tr-let) (syntax/loc stx let))])
    (syntax-parse stx
     [(_ ([name ann ... e:~>]) . body)
      (define e-φ (φ #'e.~>))
      (log-ttt-debug "(let ~a ~a)" #'name e-φ)
      (quasisyntax/loc stx
        (+let ([name ann ... e.~>])
          (let-syntax ([name (make-rename-transformer (⊢ #'name '#,e-φ))])
            . body)))]
     [(_ arg* ...)
      (syntax/loc stx (+let arg* ...))])))

(define-syntax (-let* stx)
  (syntax-case stx ()
   [(_ ([e* ...]) . body)
    (syntax/loc stx
      (--let ([e* ...]) . body))]
   [(_ ([e* ...] more ...) . body)
    (syntax/loc stx
      (--let ([e* ...])
        (-let* (more ...)
          . body)))]
   [(_ . e*)
    (syntax/loc stx
      (--let . e*))]))

(define-syntax (-let stx)
  (syntax-case stx ()
   [(_ ([e* ...]) . body)
    (syntax/loc stx
      (--let ([e* ...]) . body))]
   [(_ ([v ann ... b] more ...) . body)
    (syntax/loc stx
      (--let ([tmp ann ... b])
        (-let* (more ...)
        (--let ([v ann ... tmp]) . body))))]
   [(_ . e*)
    (syntax/loc stx
      (--let . e*))]))

(define-syntax (-define stx)
  (with-syntax ([+define (if (syntax-local-typed-context?) (syntax/loc stx tr-define) (syntax/loc stx define))])
    (syntax-parse stx
     [(_ name:id ann* ... e)
      #:with e~
        ;; TODO 2016-10-30 : `class` and `class*` should not be an issue
        ;; Here is a small program using `class`:
        ;;     #lang racket/base
        ;;     (require trivial racket/class)
        ;;     (define yo (class object% (super-new) (define/public (f x) this)))
        ;; Compiling this program gives:
        ;;     lambda: expanded syntax not in its original lexical context
        ;;     (extra bindings or scopes in the current context) in:
        ;;     (lambda (local-accessor local-mutator f1) ....
        ;; Local expanding `(lambda (name) e)` instead "works", but doesn't
        ;;  attach datum properties for `e`.
        (parameterize ([*STOP-LIST* (list* (format-id stx "class")
                                           (format-id stx "class*")
                                           (syntax/loc stx name)
                                           (*STOP-LIST*))])
          (syntax-parse #'e [e:~> #'e.~>]))
      (define e-φ (φ #'e~))
      (free-id-table-set! φ-tbl #'name e-φ)
      (log-ttt-debug "(define ~a ~a)" #'name e-φ)
      (syntax/loc stx
        (+define name ann* ... e~))]
     [(_ . e*)
      (syntax/loc stx
        (+define . e*))])))

