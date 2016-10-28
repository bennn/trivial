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
     [(_ ([name ann ... e:~>]) body ...)
      (define e-φ (φ #'e.~>))
      (log-ttt-debug "(let ~a ~a)" #'name e-φ)
      (quasisyntax/loc stx
        (+let ([name ann ... e.~>])
          (let-syntax ([name (make-rename-transformer (⊢ #'name '#,e-φ))])
            body ...)))]
     [(_ arg* ...)
      (syntax/loc stx (+let arg* ...))])))

(define-syntax (-let* stx)
  (syntax-case stx ()
   [(_ ([v b]) . body)
    (syntax/loc stx
      (--let ([v b]) . body))]
   [(_ ([v b] [vv bb] ...) . body)
    (syntax/loc stx
      (--let ([v b])
        (-let* ([vv bb] ...)
          . body)))]
   [(_ . e*)
    (syntax/loc stx
      (--let . e*))]))

(define-syntax (-let stx)
  (syntax-case stx ()
   [(_ ([v b]) . body)
    (syntax/loc stx
      (--let ([v b]) . body))]
   [(_ ([v b] [vv bb] ...) . body)
    (syntax/loc stx
      (--let ([tmp b])
        (-let* ([vv bb] ...)
        (--let ([v tmp]) . body))))]
   [(_ . e*)
    (syntax/loc stx
      (--let . e*))]))

(define-syntax (-define stx)
  (with-syntax ([+define (if (syntax-local-typed-context?) (syntax/loc stx tr-define) (syntax/loc stx define))])
    (syntax-parse stx
     [(_ nm:id ann* ... e)
      #:with e~
        (parameterize ([*STOP-LIST* (cons (syntax/loc stx name) (*STOP-LIST*))])
          (syntax-parse #'e [e:~> #'e.~>]))
      (define e-φ (φ #'e~))
      (free-id-table-set! φ-tbl #'nm e-φ)
      (log-ttt-debug "(define ~a ~a)" #'nm e-φ)
      (syntax/loc stx
        (+define nm ann* ... e~))]
     [(_ . e*)
      (syntax/loc stx
        (+define . e*))])))

