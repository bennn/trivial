#lang typed/racket/base

;; Track procedure arity
;; Applications:
;; - 
;; - vectorized ops
;; - (TODO) improve apply/map? ask Leif

(provide
  curry:
)

;; -----------------------------------------------------------------------------

(require
  (for-syntax
    typed/racket/base
    racket/syntax
    syntax/id-table
    syntax/parse
    syntax/stx
    trivial/private/common
))

;; =============================================================================

(begin-for-syntax (define-syntax-class procedure/arity
  #:attributes (expanded arity)
  (pattern e
   #:with e+ (expand-expr #'e)
   #:with a (parse-procedure-arity #'e+)
   #:when (syntax-e #'a)
   #:attr expanded #'e+
   #:attr arity #'a)
))

;; -----------------------------------------------------------------------------

(define-syntax (curry: stx)
  (syntax-parse stx
   [(_ p:procedure/arity)
    #:with (x* ...) (for/list ([_i (in-range (syntax-e #'p.arity))]) (gensym))
    #:with p+ (for/fold ([e (quasisyntax/loc stx (p #,@#`#,(reverse (syntax-e #'(x* ...)))))])
                        ([x (in-list (syntax-e #'(x* ...)))])
                (quasisyntax/loc stx
                  (lambda (#,x) #,e)))
    (syntax/loc stx p+)]
   [_
    (raise-user-error 'curry "Fail ~a" (syntax->datum stx))]))

(define-for-syntax id+procedure-arity (make-free-id-table))
(define-for-syntax procedure-arity-key 'procedure:arity)

;; -----------------------------------------------------------------------------

(define-for-syntax (parse-procedure-arity stx)
  (cond
   [(syntax-property stx procedure-arity-key)
    => (lambda (x) x)]
   [(identifier? stx)
    (free-id-table-ref id+procedure-arity stx #f)]
   [else
    (syntax-parse stx #:literals (#%plain-lambda)
     [(#%plain-lambda (x*:id ...) e* ...)
      (length (syntax-e #'(x* ...)))]
     ;; TODO polydots, keywords, optional args
     ;; TODO standard library functions
     [_ #f])]))

