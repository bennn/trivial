#lang typed/racket/base

;; TODO do not use this library, it's just for demonstration
;; TODO confusing row/column

(provide
  start-transaction
  rollback-transaction
  query-exec
  Connection

  ;(rename-out [quasiquote DB]) ;; TODO try using struct types

  let-schema:
  define-schema:
  let-connection:
  define-connection:
  postgresql-connect:

  ;query-exec:
  query-row:
  ;query-maybe-row:

  ;; define schema
  ;; start connection
  ;; query-exec

  ;; --
  (for-syntax
    connection-key)
)

;; -----------------------------------------------------------------------------

(require
  (only-in trivial/private/vector vector-length-key)
  (for-syntax
    typed/racket/base
    racket/syntax
    ;; --- ???
    syntax/parse
    syntax/stx
    trivial/private/common
    trivial/private/db/schema
    trivial/private/db/query
))

(require/typed db
  (#:opaque Connection connection?)
  (postgresql-connect (->* [#:user String #:database String] [] Connection))
  (query-row (-> Connection String Any * (Vectorof Any)))
  (query-maybe-row (-> Connection String Any * (Option (Vectorof Any))))
  ;; ---
  (start-transaction (-> Connection Void))
  (rollback-transaction (-> Connection Void))
  (query-exec (-> Connection String Any * Void))
)

;; =============================================================================

(define-syntax define-schema: (make-keyword-alias 'define schema-def))
(define-syntax let-schema: (make-keyword-alias 'let schema-let))

(begin-for-syntax
  (define (connection-parser stx)
    ;; Connections have no primitive form -- need to use a wrapped API function
    #f)

  (define-values (connection-key connection? connection-def connection-let)
    (make-value-property 'db:connection connection-parser))
  (define-syntax-class/predicate connection/expand connection?)
)

(define-syntax define-connection: (make-keyword-alias 'define connection-def))
(define-syntax let-connection: (make-keyword-alias 'let connection-let))

;; -----------------------------------------------------------------------------

(define-syntax (postgresql-connect: stx)
  (syntax-parse stx
   [(_ s:schema/expand e* ...)
    (syntax-property
      (syntax/loc stx (postgresql-connect e* ...))
      connection-key
      #'s.evidence)]))

;; TODO query-maybe-row

(define-syntax query-row: (make-alias #'query-row
  (lambda (stx) (syntax-parse stx
   [(_ c:connection/expand q:query/expand arg* ...)
    (define schema (syntax->datum #'c.evidence))
    (define-values (maybe-row* table condition*)
      (apply values (syntax->datum #'q.evidence)))
    ;; -- Check connection vs. schema
    (define tbl-schema (table-mem schema table))
    (unless tbl-schema
      (raise-syntax-error 'query-row: "Unknown table" (syntax->datum stx) table))
    (define row* (resolve-wildcard tbl-schema maybe-row*))
    (when (null? row*)
      (raise-syntax-error 'query-row: "Empty selection" (syntax->datum stx) 'q.expanded))
    (define result-type*
      (for/list ([r (in-list row*)])
        (or (row-mem tbl-schema r)
            (raise-syntax-error 'query-row "Unknown row" (syntax->datum stx) r))))
    (define type* (condition*->type* schema condition* #:src stx))
    ;; -- Check number of arguments
    (let ([num-expected (length type*)]
          [num-actual   (length (syntax-e #'(arg* ...)))])
      (unless (= num-expected num-actual)
        (apply raise-arity-error
          'query-row:
          num-expected
          (map syntax->datum (syntax-e #'(arg* ...))))))
    (define (id->type id) (format-id stx "~a" id))
    (with-syntax ([(t* ...) (map id->type type*)]
                  [vec-stx (format-id stx "Vector")]
                  [(r-t* ...) (map id->type result-type*)])
      (syntax-property
        (syntax/loc stx
          (cast (query-row c.expanded q.expanded (ann arg* t*) ...)
                (vec-stx r-t* ...)))
        vector-length-key
        (length result-type*)))]
   [_ #f]))))

