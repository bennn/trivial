#lang racket/base

;; Implement GENERIC elaboration rules,
;;  propagating prop. environments φ

(provide
  (rename-out
    [-let let]
    [-define define]))
  ;tailored-out let

;; -----------------------------------------------------------------------------

(require
  (prefix-in tr: typed/racket/base)
  (for-syntax
    trivial/private/common
    racket/base
    syntax/parse
    syntax/id-table))

;; =============================================================================

(define-syntax (-let stx)
  (syntax-parse stx
    [(let ([name* e*:~>] ...) body ...)
     #:with (φ* ...) (for/list ([e (in-list (syntax-e #'(e*.~> ...)))])
                       (printf "looking up ~a in phi\n" (syntax->datum e))
                       (φ e))
     (when (*TRIVIAL-LOG*)
       (ttt-log stx "let got anmes ~a es ~a maps ~a" #'(name* ...) #'(e*.~> ...) #'(φ* ...)))
     (quasisyntax/loc stx
       (tr:let ([name* e*.~>] ...)
         (tr:let-syntax ([name* (make-rename-transformer
                                  (⊢ #'name* 'φ*))] ...)
           body ...)))]
    [(let arg* ...)
     #'(tr:let arg* ...)]))

(define-syntax (-define stx)
  (syntax-parse stx
   [(_ nm:id e)
    #:with e~
      (parameterize ([*STOP-LIST* (cons (syntax/loc stx name) (*STOP-LIST*))])
        (syntax-parse #'e [e:~> #'e.~>]))
    (free-id-table-set! φ-tbl #'nm (φ #'e~))
    (when (*TRIVIAL-LOG*)
      (ttt-log stx "define"))
    #'(tr:define nm e~)]
   [(_ . e*)
    #'(tr:define . e*)]))
