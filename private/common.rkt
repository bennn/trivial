#lang racket/base

;; Common helper functions
;; TODO make-set!-transformer

(provide

  quoted-stx-value?
  ;; (-> Any (U #f Syntax))
  ;; If the argument is a syntax object representing a quoted datum `v`,
  ;;  return `v`.
  ;; Otherwise, return #f.

  define-syntax-class/predicate
  ;; TODO

  lift-predicate
  ;; TODO

  make-value-property
  ;; TODO

  make-alias
  make-keyword-alias
  ;; TODO
)

(require
  racket/syntax
  syntax/parse
  syntax/id-table
  (for-syntax (only-in typed/racket/base let let-syntax #%app))
  (for-template
    (prefix-in r: (only-in racket/base quote))
    (prefix-in tr: (only-in typed/racket/base quote))))

;; =============================================================================

(define-syntax-rule (define-syntax-class/predicate id p?)
  (define-syntax-class id
   #:attributes (evidence expanded)
   (pattern e
    #:with e+ (expand-expr #'e)
    #:with p+ (p? #'e+)
    #:when (syntax-e #'p+)
    #:attr evidence #'p+
    #:attr expanded #'e+)))

(define (expand-expr stx)
  (local-expand stx 'expression '()))

(define (quoted-stx-value? stx)
  (and
    (syntax? stx)
    (syntax-parse stx #:literals (r:quote tr:quote) #:datum-literals (quote)
     [((~or r:quote tr:quote quote) v)
      (syntax-e #'v)]
     [else #f])))

(define (lift-predicate p?)
  (lambda (stx)
    (cond
     [(p? stx) stx]
     [(p? (syntax-e stx)) (syntax-e stx)]
     [(p? (quoted-stx-value? stx))
      stx]
     [else #f])))

;; In:
;; - name : Symbol, like format-spec or vector-length or db-schema
;; - parser : (Syntax -> Value)
;;            Syntax is anything, need to filter yourself
;;            Value is the important type++ data
;; Out:
;; - (Syntax -> (Option Syntax)) x3
;;   1st is recognizer, cooperates with define & let
;;   2nd is define form
;;   3rd is let form
;; - id table
;; - syntax property key
;; Put transformers here too? Then the id table never escapes
(define (make-value-property sym parser)
  (define key (gensym sym))
  (define tbl (make-free-id-table))
  (define f-parse
    (lambda (stx)
      (let ([v (syntax-property stx key)])
        (cond
         [v                  v]
         [(identifier? stx)  (free-id-table-ref tbl stx #f)]
         [else               (parser stx)]))))
  (define f-define
    (lambda (stx)
      (syntax-parse stx
       [(_ name:id v)
        #:with v+ (expand-expr (syntax/loc stx v))
        #:when (syntax-e (syntax/loc stx v+))
        #:with m (f-parse (syntax/loc stx v+))
        #:when (syntax-e (syntax/loc stx m))
        #:with define-stx (format-id stx "define")
        (free-id-table-set! tbl #'name (syntax-e #'m))
        (syntax/loc stx
          (define-stx name v+))]
       [_ #f])))
  (define f-let
    (lambda (stx)
      (syntax-parse stx
       [(_ ([name*:id v*] ...) e* ...)
        #:with (v+* ...) (map expand-expr (syntax-e (syntax/loc stx (v* ...))))
        #:with (m* ...) (map f-parse (syntax-e (syntax/loc stx (v+* ...))))
        #:when (andmap syntax-e (syntax-e (syntax/loc stx (m* ...))))
        #:with let-stx (format-id stx "let")
        #:with let-syntax-stx (format-id stx "let-syntax")
        (quasisyntax/loc stx
          (let-stx ([name* v+*] ...)
            (let-syntax-stx ([name* (make-rename-transformer
                                      (syntax-property #'name* '#,key 'm*))] ...)
              e* ...)))]
       [_ #f])))
  (values
    key
    f-parse
    f-define
    f-let))

(define ((make-alias id-sym parser) stx)
  (or (parser stx)
    (syntax-parse stx
     [_:id
      #:with id-stx (format-id id-sym "~a" (syntax-e id-sym))
      (syntax/loc stx id-stx)]
     [(_ e* ...)
      #:with id-stx (format-id id-sym "~a" (syntax-e id-sym))
      #:with app-stx (format-id stx "#%app")
      (syntax/loc stx (app-stx id-stx e* ...))])))

(define ((make-keyword-alias id-sym parser) stx)
  (or (parser stx)
    (syntax-parse stx
     [(_ e* ...)
      #:with id-stx (format-id stx "~a" id-sym)
      (syntax/loc stx (id-stx e* ...))])))
