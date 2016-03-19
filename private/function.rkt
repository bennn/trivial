#lang typed/racket/base

;; TODO get type from a lambda AFTER expansion
;
;(require
; (for-syntax
;   (only-in typed-racket/private/syntax-properties plambda-property)))
;
;; -----------------------------------------------------------------------------

;; Track procedure arity
;; Applications:
;; - 
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

;(require
;    (prefix-in tr: typed/racket/base)
;    (prefix-in r: (only-in racket/base quote))
; (for-syntax
;  syntax/id-table))

;; =============================================================================

(begin-for-syntax
  (define TYPE-KEY 'type-label)

  (define (parse-procedure-arity stx)
    (syntax-parse stx #:literals (: #%plain-lambda lambda)
     [(#%plain-lambda (x*:id ...) e* ...)
      (syntax/loc stx (x* ...))]
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
    #:with p+ (for/fold ([e (quasisyntax/loc stx (p.expanded #,@#'p.evidence))])
                        ([x (in-list (reverse (syntax-e #'p.evidence)))])
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
    #:with Listof-stx (format-id stx "Listof")
    #:with (e+* ...)
      (for/list ([t (in-list (syntax-e #'p.evidence))]
                 [e (in-list (syntax-e #'(e* ...)))])
        (quasisyntax/loc stx (ann #,e (Listof-stx #,t))))
    (syntax/loc stx (map p.expanded e+* ...))]
   [(_ p e* ...)
    ;; TODO -- this case should be subsumed by the last
    #:with p+ (expand-expr #'p)
    #:with evi (fun? #'p+)
    #:when (syntax-e #'evi)
    #:when
      (let ([num-expected (length (syntax-e #'evi))]
            [num-actual (length (syntax-e #'(e* ...)))])
        (unless (= num-expected num-actual)
          (apply raise-arity-error
            'map:
            num-expected
            (map syntax->datum (syntax-e #'(e* ...))))))
    ;; --
    #:with Listof-stx (format-id stx "Listof")
    #:with (e+* ...)
      (for/list ([t (in-list (syntax-e #'evi))]
                 [e (in-list (syntax-e #'(e* ...)))])
        ;; TODO stop using format-id
        (quasisyntax/loc stx (ann #,e (Listof-stx #,(format-id stx "~a" t)))))
    (syntax/loc stx (map p+ e+* ...))]))))

