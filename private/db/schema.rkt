#lang racket/base

;; Reflective datatype for database schemas

;; -- schema = ((DB-NAME (ROW-TYPE ...))
;;              ...)


(provide
  schema-def
  schema-let
)

(require
  trivial/private/common
  (only-in racket/string string-split)
  (for-syntax
    typed/racket/base
    racket/syntax
    syntax/parse)
)

;; =============================================================================

;(define-type TypeSymbol Symbol)
;(define-type RowSchema (Pairof Symbol TypeSymbol))
;(define-type TableSchema (List Symbol RowSchema))
;(define-type DbSchema (Listof TableSchema))

(define (schema-parser stx)
  (define x** (cadr (syntax->datum stx)))
  (cond
   [(and (list? x**)
         (for/and ([x* (in-list x**)])
           (and (list? x*)
                (= 2 (length x*))
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

;; -----------------------------------------------------------------------------
;; sql basics, belongs in a new file?

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
  (define str (if (string? s) s (symbol->string s)))
  (and
    (= 2 (string-length str))
    (eq? #\$ (string-ref str 0))
    (string->number (string (string-ref str 1)))))

;; -----------------------------------------------------------------------------

;(: table-mem (-> DbSchema TableName (Option TableSchema)))
(define (table-mem schema tbl)
  (for/first ;: (Option TableSchema)
             ([tbl-schema (in-list schema)]
              #:when (eq? tbl (car tbl-schema)))
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

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (check-false* schema-parser
   [#'(quote #f)]
   [#'(quote "hello")]
   [#'(quote (a b c d))]
  )

  (check-true* (lambda (x) (and (schema-parser x) #t))
   [#'(quote ())]
   [#'(quote ((Foo ())))]
   [#'(quote ((Foo ((Bar . Baz)))))]
  )

  (check-apply* symbol-ci=?
   ['a 'a
    => #t]
   ['a 'A
    => #t]
   ['yellow 'YeLLOW
    => #t]
   ['wait 'forME
    => #f]
   ['x 'y
    => #f]
   ["A" 'A
    => #f]
   [315 "bage"
    => #f]
  )

  (check-apply* select?
   ['select
    => #t]
   ['SELECT
    => #t]
   ['yolo
    => #f]
  )

  (check-apply* from?
   ['from
    => #t]
   ['FROM
    => #t]
   ['yolo
    => #f]
  )

  (check-apply* where?
   ['where
    => #t]
   ['WHERE
    => #t]
   ['yolo
    => #f]
  )

  (check-apply* and?
   ['and
    => #t]
   ['AND
    => #t]
   ['yolo
    => #f]
  )

  (check-apply* sql-variable?
   ["$1"
    => 1]
   ['$1
    => 1]
   ["$125"
    => #f]
   ['$555
    => #f]
   ['wepa
    => #f]
  )
  (check-exn exn:fail:contract?
    (lambda () (sql-variable? 3)))

  (check-apply* table-mem
   ['((a ((b . c)))) 'a
    =>  '((b . c))]
   ['((a ((a1 . a2)
          (a3 . a4)))
      (b ((b1 . b2))))
    'b
    => '((b1 . b2))]
  )

  (check-apply* row-mem
   ['((a1 . a2) (a3 . a4))
    'a3
    => 'a4]
  )

  (check-apply* resolve-wildcard
   ['((b . c) (d . e))
    'x
    => '(x)]
   ['((b . c) (d . e))
    '(a i r)
    => '(a i r)]
   ['((b . c) (d . e))
    '*
    => '(b d)]
  )

  (let ([sc '((a ((a1 . a2)))
              (b ((b1 . b2) (b3 . b4))))])
    (check-apply* row-ref->type
     [sc
      "a.a1"
      => 'a2]
     [sc
      "a1"
      => 'a2]
     [sc
      "b.b3"
      => 'b4]
     [sc
      "b.b5"
      => #f]
     [sc
      "x"
      => #f]
    ))
  (check-exn #rx"internal-error"
    (lambda () (row-ref->type '() "..")))
  (check-exn #rx"internal-error"
    (lambda () (row-ref->type '() "a.b.c")))

  (check-apply* global-row-mem
   ['((a ((b . c))))
    'b
    => 'c]
   ['()
    'a
    => #f]
   ['((a ((b . c))))
    'a
    => #f]
   ['((a ((b . c)))
      (a2 ((b . c))))
    'b
    => #f]
  )

  (let ([sc '((a ((b . c)))
              (d ((e . f))))])
    (check-apply* condition*->type*
     [sc
      '(("a.b" . "$1"))
      #:src #'f
      => '(c)]
     [sc
      '(("b" . "$1"))
      #:src #'f
      => '(c)]
     [sc
      '(("b" . "yolo"))
      #:src #'f
      => '()]
    )
    (check-exn #rx"Missing query parameter"
      (lambda () (condition*->type* sc '(("a.b" . "$3")) #:src #'#f)))
  )
)
