#lang typed/racket/base

(provide
  (for-syntax DB-dom Connection-dom)
  (rename-out
    [-sqlite3-connect sqlite3-connect]
    [-mysql-connect mysql-connect]
    [-query-row query-row])

;  start-transaction
;  rollback-transaction
;  query-exec
;  Connection
;  ;(rename-out [quasiquote DB]) ;; TODO try using struct types
;  let-schema:
;  define-schema:
;  let-connection:
;  define-connection:
;  postgresql-connect:
;  ;query-exec:
;  ;query-maybe-row:
;  ;; define schema
;  ;; start connection
;  ;; query-exec
)

;; -----------------------------------------------------------------------------

(require
  trivial/private/tailoring
  (for-syntax
    (rename-in trivial/private/sequence-domain
      [vector-domain V-dom]
      [I-dom->vector-domain I->V])
    racket/base
    racket/syntax
    syntax/parse
    syntax/stx
    typed/untyped-utils
    trivial/private/common
    trivial/private/db/schema
    trivial/private/db/query
))

(require/typed db
  (#:opaque Connection connection?)
  (mysql-connect (->* [#:user String #:database String] [] Connection))
  (sqlite3-connect (->* [#:database String] [] Connection))
  (query-row (-> Connection String Any * (Vectorof Any)))
)

;; =============================================================================

(define-for-syntax DB-dom
  (make-abstract-domain DB
   [s:str
    (query-parser #'s)]))

(define-for-syntax Connection-dom
  (make-abstract-domain Conn
   [s:expr
    (schema-parser #'s)]))

;; (define-for-syntax query-key (gensym 'query))
;; (define-for-syntax connection-key (gensym 'schema))

;; -----------------------------------------------------------------------------

(define-syntax (-mysql-connect stx)
  (syntax-parse stx
   [(_ #:user user:str
       #:database database:str
       #:schema s:expr)
    #:with schema (schema-parser #'s)
    (⊢
      (syntax/loc stx (mysql-connect #:user user #:database database))
      (φ-set (φ-init) Connection-dom #'schema))]))

(define-syntax (-sqlite3-connect stx)
  (syntax-parse stx
   [(_ #:database database:str
       #:schema s:expr)
    #:with schema (schema-parser #'s)
    (⊢
      (syntax/loc stx (sqlite3-connect #:database database))
      (φ-set (φ-init) Connection-dom #'schema))]))

(define-syntax (-query-row stx)
  (syntax-parse stx
   [(_ c:~> q:~> arg* ...)
    (define schema-val (φ-ref (φ #'c.~>) Connection-dom))
    (define query-val (φ-ref (φ #'q.~>) DB-dom))
    (cond
      [(not (and schema-val query-val))
       (syntax/loc stx
         (query-row c.~> q.~> arg* ...))]
      [else
       (define-values (maybe-row* table condition*) (apply values query-val))
       (define tbl-schema (table-mem schema-val table))
       (unless tbl-schema
         (raise-syntax-error 'query-row "Unknown table" (syntax->datum stx) table))
       (define row* (resolve-wildcard tbl-schema maybe-row*))
       (when (null? row*)
         (raise-syntax-error 'query-row "Empty selection" (syntax->datum stx) 'q.~>))
       (define result-type*
         (for/list ([r (in-list row*)])
           (or (row-mem tbl-schema r)
               (raise-syntax-error 'query-row "Unknown column" (syntax->datum stx) r))))
       (define type* (condition*->type* schema-val condition* #:src stx))
       ;; -- Check number of arguments
       (define num-expected (length type*))
       (let ([num-actual   (length (syntax-e #'(arg* ...)))])
         (unless (= num-expected num-actual)
           (apply raise-arity-error
             'query-row
             num-expected
             (map syntax->datum (syntax-e #'(arg* ...))))))
       (define (id->type id) (format-id stx "~a" id))
       (with-syntax ([(t* ...) (map id->type type*)]
                     [vec-stx (format-id stx "Vector")]
                     [(r-t* ...) (map id->type result-type*)])
         (⊢
           (syntax/loc stx
             (cast (query-row c.~> q.~> (ann arg* t*) ...)
                   (vec-stx r-t* ...)))
           (φ-set (φ-init) V-dom (I->V num-expected)))) ])]
   [_ #f]))

