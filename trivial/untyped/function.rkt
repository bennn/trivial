#lang racket/base

;; TODO
;;  map passing, but cury failig; can't make a lambda like I'd like to

;; -----------------------------------------------------------------------------

;; Track procedure arity
;; Applications:
;; - vectorized ops
;; - (TODO) improve apply/map? ask Leif
;; - TODO get types, not arity

(provide
  curry:
  map:

  ;; --
  (for-syntax
    fun-define
    fun-let)
)

;; -----------------------------------------------------------------------------

(require
  (for-syntax
    typed/racket/base
    syntax/parse
    racket/syntax
    trivial/private/common
))

;; =============================================================================

(begin-for-syntax
  (define TYPE-KEY 'type-label)

  (define (formal->type x)
    #f)

  (define (parse-procedure-arity stx)
    (syntax-parse stx #:literals (: #%plain-lambda lambda)
     [(#%plain-lambda (x*:id ...) e* ...)
      (map formal->type (syntax-e #'(x* ...)))]
     ;; TODO polydots, keywords, optional args
     ;; TODO standard library functions
     [_ #f]))

  (define-values (arity-key fun? fun-define fun-let)
    (make-value-property 'procedure:arity parse-procedure-arity))

  (define-syntax-class/predicate procedure/arity fun?)
)

;; -----------------------------------------------------------------------------

(define-syntax (curry: stx)
  (syntax-parse stx
   [(_ p:procedure/arity)
    #:with x* (for/list ([_t (in-list (syntax-e #'p.evidence))]) (gensym))
    #:with p+ (for/fold ([e (quasisyntax/loc stx (p.expanded #,@#'x*))])
                        ([x (in-list (reverse (syntax-e #'x*)))])
                (quasisyntax/loc stx
                  (lambda (#,x) #,e)))
    (syntax/loc stx p+)]
   [_
    (raise-user-error 'curry "Fail at: ~a" (syntax->datum stx))]))

;; TODO try the other direction, inferring type from arguments.
;;   (may not be practical here, may need to be inside TR)
(define-syntax map: (make-alias #'map
  (lambda (stx) (syntax-parse stx
   [(_ p:procedure/arity e* ...)
    ;; --
    #:when
      (let ([num-expected (length (syntax-e #'p.evidence))]
            [num-actual (length (syntax-e #'(e* ...)))])
        (unless (= num-expected num-actual)
          (apply raise-arity-error
            'map:
            num-expected
            (map syntax->datum (syntax-e #'(e* ...))))))
    ;; --
    #:with (e+* ...)
      (for/list ([e (in-list (syntax-e #'(e* ...)))])
        (quasisyntax/loc stx #,e))
    (syntax/loc stx (map p.expanded e+* ...))]))))

