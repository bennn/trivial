#lang typed/racket/base

;; TODO do not use this library, it's just for demonstration

(provide
  (all-from-out db)
 
  ;(rename-out [quasiquote DB]) ;; TODO try using struct types

  define-schema:
  define-connection:
  postgresql-connect:

  ;query-exec:
  query-row:
  ;query-maybe-row:

  ;; define schema
  ;; start connection
  ;; query-exec
)

;; -----------------------------------------------------------------------------

(require
  (only-in trivial/private/vector vector-length-key)
  (for-syntax
    typed/racket/base
    racket/syntax
    (only-in racket/string string-split)
    ;; --- query
    (only-in racket/port with-input-from-string)
    (only-in racket/format ~a)
    racket/match
    (for-syntax syntax/parse racket/syntax typed/racket/base)
    ;; --- ???
    syntax/parse
    syntax/stx
    trivial/private/common
))

(require/typed db
  (#:opaque Connection connection?)
  (postgresql-connect (->* [#:user String #:database String] [] Connection))
  (query-row (-> Connection String Any * (Vectorof Any)))
  (query-maybe-row (-> Connection String Any * (Option (Vectorof Any))))
)

;; =============================================================================

(begin-for-syntax

  ;; -- schema = ((DB-NAME (ROW-TYPE ...))
  ;;              ...)

  (define (schema-parser stx)
    (define x** (cadr (syntax->datum stx)))
    (cond
     [(and (list? x**)
           (for/and ([x* (in-list x**)])
             (and (= 2 (length x*))
                  (symbol? (car x*))
                  (list? (cadr x*))
                  (for/and ([r (in-list (cadr x*))])
                    (and (pair? r)
                         (symbol? (car r))
                         (symbol? (cdr r)))))))
      x**]
     [else #f]))

  (define-values (schema-key schema? schema-def schema-let)
    (make-value-property 'db:schema schema-parser))
  (define-syntax-class/predicate schema/expand schema?)

  (define (table-mem schema db)
    (for/first ([tbl-schema (in-list schema)]
                #:when (eq? db (car tbl-schema)))
      (cadr tbl-schema)))

  (define (row-mem tbl-schema row)
    (for/first ([row-schema (in-list tbl-schema)]
                #:when (eq? (car row-schema) row))
      (cdr row-schema)))

  (define (resolve-wildcard tbl-schema row)
    (cond
     [(eq? row '*)
      (map car tbl-schema)]
     [(list? row)
      row]
     [else
      (list row)]))

  (define (row-ref->type schema qrow)
    (define q* (string-split qrow "."))
    (case (length q*)
     [(1)
      ;; Make sure that row exists SOMEWHERE in table
      (global-row-mem schema (string->symbol (car q*)))]
     [(2)
      ;; Have table name + row name, make sure they match
      (let ([tbl (table-mem schema (string->symbol (car q*)))])
        (and tbl (row-mem tbl (string->symbol (cadr q*)))))]
     [else
      (error 'internal-error "Failed to parse query-row '~a'" qrow)]))

  (define (global-row-mem schema row)
    (let loop ([acc #f]
               [schema schema])
      (cond
       [(null? schema)
        acc]
       [(row-mem (cadr (car schema)) row)
        => (lambda (t)
        (if acc #f (loop t (cdr schema))))]
       [else
        (loop acc (cdr schema))])))

  (define (condition*->type* schema condition* #:src stx)
    (define unsorted
      (for/fold ([acc '()])
                ([condition (in-list condition*)])
        (define typ (row-ref->type schema (car condition)))
        (unless typ
          (raise-syntax-error 'query-row:
            "Failed to resolve type for row" (syntax->datum stx) condition))
        (define val (cdr condition))
        (define varnum (sql-variable? val))
        (if varnum
          (cons (cons typ varnum) acc)
          acc)))
    (for/list ([typ+num (sort unsorted string<? #:key cdr)]
               [num (in-naturals 1)])
      (unless (= (cdr typ+num) num)
        (raise-syntax-error 'query-row:
          (format "Missing query parameter '~a'" num)
          (syntax->datum stx)))
      (car typ+num)))

  ;; --------------------------------------------------------------------------

  (define (connection-parser stx)
    ;; Connections have no primitive form -- need to use a wrapped API function
    #f)

  (define-values (connection-key connection? connection-def connection-let)
    (make-value-property 'db:connection connection-parser))
  (define-syntax-class/predicate connection/expand connection?)

  ;; --------------------------------------------------------------------------
  ;; -- query

  (define (query-parser stx)
    (define str (if (string? (syntax-e stx)) (syntax-e stx) (quoted-stx-value? stx)))
    (and
     (string? str)
     (match (with-input-from-string (string-append "(" str ")") read)
      [(list (? select?) sel (? from?) database rest* ...)
       (define condition* (condition-parser rest*))
       (list sel database condition*)]
      [_ #f])))

  (define (symbol-ci=? s1 s2)
    (and
     (symbol? s1)
     (symbol? s2)
     (string-ci=? (symbol->string s1) (symbol->string s2))))

  (define-syntax (define-sql-keyword-predicate stx)
    (syntax-parse stx
     [(_ kwd*:id ...)
      #:with (kwd?* ...) (for/list ([kwd (in-list (syntax-e #'(kwd* ...)))])
                           (format-id stx "~a?" (syntax-e kwd)))
      (syntax/loc stx
        (begin (define (kwd?* v) (symbol-ci=? v 'kwd*)) ...))]))

  (define-sql-keyword-predicate
    select
    from
    where
    and)

  ;; Check for query parameters. Currently only for Postgres.
  (define (sql-variable? s)
    (define str (~a s))
    (and
      (= 2 (string-length str))
      (eq? #\$ (string-ref str 0))
      (string->number (string (string-ref str 1)))))

  (define (condition-parser v*)
    (let loop ([v* v*])
      (match v*
       ['()
        '()]
       [(list (or (? where?) (? and?)) db+row '= v rest* ...)
        (cons (cons (~a db+row) (~a v))
              (loop rest*))]
       [(cons _ rest*)
        (loop rest*)])))

  (define-values (query-key query? query-def query-let)
    (make-value-property 'db:query query-parser))
  (define-syntax-class/predicate query/expand query?)

)

(define-syntax define-schema: (make-keyword-alias 'define schema-def))
(define-syntax let-schema: (make-keyword-alias 'let schema-let))
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

(define-syntax query-row: (make-alias #'query-row
  (lambda (stx) (syntax-parse stx
   [(_ c:connection/expand q:query/expand arg* ...)
    (define schema (syntax->datum #'c.evidence))
    (define-values (maybe-row* table condition*)
      (apply values (syntax->datum #'q.evidence)))
    ;; -- Check connection vs. schema
    (define tbl-schema (table-mem schema table))
    (unless tbl-schema
      (raise-syntax-error 'query-row "Unknown table" (syntax->datum stx) table))
    (define row* (resolve-wildcard tbl-schema maybe-row*))
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

