#lang typed/racket/base

;; Track procedure arity
;; Applications:
;; - 
;; - vectorized ops
;; - (TODO) improve apply/map? ask Leif
;; - TODO get types, not arity

(provide
  curry:
)

;; -----------------------------------------------------------------------------

(require
  (for-syntax
    typed/racket/base
    syntax/parse
    trivial/private/common
))

;; =============================================================================

(begin-for-syntax
  (define (parse-procedure-arity stx)
    (syntax-parse stx #:literals (#%plain-lambda)
     [(#%plain-lambda (x*:id ...) e* ...)
      (length (syntax-e #'(x* ...)))]
     ;; TODO polydots, keywords, optional args
     ;; TODO standard library functions
     [_ #f]))

  (define-values (arity-key proc? define-proc let-proc)
    (make-value-property 'procedure:arity parse-procedure-arity))
  (define-syntax-class/predicate procedure/arity proc?)
)

;; -----------------------------------------------------------------------------

(define-syntax (curry: stx)
  (syntax-parse stx
   [(_ p:procedure/arity)
    #:with (x* ...) (for/list ([_i (in-range (syntax-e #'p.evidence))]) (gensym))
    #:with p+ (for/fold ([e (quasisyntax/loc stx (p #,@#`#,(reverse (syntax-e #'(x* ...)))))])
                        ([x (in-list (syntax-e #'(x* ...)))])
                (quasisyntax/loc stx
                  (lambda (#,x) #,e)))
    (syntax/loc stx p+)]
   [_
    (raise-user-error 'curry "Fail ~a" (syntax->datum stx))]))

