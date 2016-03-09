#lang racket/base

;; Common helper functions

(provide

  quoted-stx-value?
  ;; (-> Any (U #f Syntax))
  ;; If the argument is a syntax object representing a quoted datum `v`,
  ;;  return `v`.
  ;; Otherwise, return #f.

  define-syntax-class/predicate
  ;; (stx-> Identifier (-> Any Boolean) SyntaxClassDef)

  make-value-property
  ;; TODO

  make-alias
  ;; TODO
)

(require
  racket/syntax
  syntax/parse
  syntax/id-table
  (for-syntax (only-in typed/racket/base let let-syntax #%app))
  (for-template (only-in typed/racket/base quote)))

;; =============================================================================

(define-syntax-rule (define-syntax-class/predicate id p?)
  (define-syntax-class id
   #:attributes (evidence expanded)
   (pattern e
    #:with e+ (expand-expr #'e)
    #:with p+ (p? #'e+)
    #:when (if (syntax-e #'p+) #t (begin (printf "ERROR we failed iwth ~a\n" (syntax->datum #'e+)) #f)) ;; TODO remove this
    #:attr evidence #'p+
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
        #:with v+ (expand-expr #'v)
        #:when (syntax-e #'v+)
        #:with m (f-parse #'v+)
        #:when (syntax-e #'m)
        (free-id-table-set! #'name (syntax-e #'m))
        (syntax/loc stx
          (define name v+))]
       [_ #f])))
  (define f-let
    (lambda (stx)
      (syntax-parse stx
       [(_ ([name*:id v*] ...) e* ...)
        #:with (v+* ...) (map expand-expr (syntax-e #'(v* ...)))
        #:when (andmap syntax-e (syntax-e #'(v+* ...)))
        #:with (m* ...) (map f-parse (syntax-e #'(v+* ...)))
        #:when (andmap syntax-e (syntax-e #'(m* ...)))
        #:with let-stx (format-id stx "let")
        #:with let-syntax-stx (format-id stx "let-syntax")
        (quasisyntax/loc stx
          (let-stx ([name* v+*] ...)
            (let-syntax-stx ([name* (make-rename-transformer
                                  (syntax-property #'name* '#,key 'm* ...))] ...)
              e* ...)))]
       [_ #f])))
  (values
    key
    f-parse
    f-define
    f-let))

(define ((make-alias id-stx parser) stx)
  (or (parser stx)
    (syntax-parse stx
     [_:id       (quasisyntax/loc stx #,id-stx)]
     [(_ e* ...) (quasisyntax/loc stx (#,id-stx e* ...))])))
