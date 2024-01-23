#lang racket/base

;; Implement generic elaboration rules,
;;  propagating prop. environments φ

(provide
  (rename-out
    [-let let]
    [-let* let*]
    ;; [-error error]
    ;; [-if if]
    [-define define]
    [-set! set!]))

;; -----------------------------------------------------------------------------

(require
  (prefix-in tr- (only-in typed/racket/base define error if let))
  (for-syntax
    trivial/private/common
    typed/untyped-utils
    racket/base
    racket/syntax
    syntax/parse
    syntax/id-set
    syntax/id-table))

;; =============================================================================

(define-syntax (--let stx)
  (with-syntax ([+let (if (syntax-local-typed-context?) (syntax/loc stx tr-let) (syntax/loc stx let))])
    (syntax-parse stx
     [(_ ([name ann ... e:~>]) . body)
      (define e-φ (φ (syntax-local-introduce #'e.~>)))
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
        ;; TODO 2016-10-30:
        ;; - debug/fix 'class misuse of method (not in application)
        ;;   - plot-area.rkt
        ;;   - mines.rkt
        ;; (both these are 'benchmark/' programs)
        (parameterize ([*STOP-LIST* (list* (format-id stx "class")
                                           (format-id stx "class*")
                                           (format-id stx "#%app")
                                           (syntax/loc stx name)
                                           (*STOP-LIST*))])
          (syntax-parse #'e [e:~> #'e.~>]))
      (define e-φ (φ #'e~))
      (log-ttt-debug "(define ~a ~a)" #'name e-φ)
      (quasisyntax/loc stx
        (begin
          (+define name ann* ... e~)
          (define-syntaxes ()
            (begin
              (free-id-table-set! φ-tbl #'name '#,e-φ)
              (values)))))]
     [(_ . e*)
      (syntax/loc stx
        (+define . e*))])))

(define-syntax (-set! stx)
  (syntax-parse stx
   [(_ name:id e:~>)
    (log-ttt-warning "the library is unsound for set!, check that ~a has not been used for unsafe optimization, in: ~a" (syntax-e #'name) (syntax->datum stx))
    (define e-φ (φ #'e.~>))
    (free-id-set-add! φ-mutated #'name)
    (syntax/loc stx (set! name e.~>))]
   [(_ . e*)
    (syntax/loc stx (set! . e*))]
   [_:id (syntax/loc stx set!)]))

;; (define-syntax (-if stx)
;;   (with-syntax ([+if (if (syntax-local-typed-context?) (syntax/loc stx tr-if) (syntax/loc stx if))])
;;     (syntax-parse stx
;;      [(_ tst thn:~> els:~>)
;;       #:with thn+ #'thn.~>
;;       #:with els+ #'els.~>
;;       (⊢ (quasisyntax/loc stx
;;            (+if tst thn+ els+))
;;          (φ-join (φ #'thn+) (φ #'els+)))])))
;; 
;; (define-syntax (-error stx)
;;   (with-syntax ([+error (if (syntax-local-typed-context?) (syntax/loc stx tr-error) (syntax/loc stx error))])
;;     (syntax-parse stx
;;      [(_ args ...)
;;       (⊢ (quasisyntax/loc stx
;;            (+error args ...))
;;          (φ-init))])))

