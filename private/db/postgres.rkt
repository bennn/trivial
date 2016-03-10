#lang racket/base

;; sql basics, belongs in a new file?

(provide
  postgres-parameter?
  ;; (-> Any (Option Natural))
  ;; If input is a Postgres parameter, return the parameter number.
  ;;  i.e $2 -> 2
  ;; Otherwise return #f.

  ;; -- also exports predicates defined with `define-sql-keyword-predicate`
)

(require
  (for-syntax
    typed/racket/base
    racket/syntax
    syntax/parse))

(define-syntax (define-sql-keyword-predicate stx)
  (syntax-parse stx
   [(_ kwd*:id ...)
    #:with (kwd?* ...) (for/list ([kwd (in-list (syntax-e #'(kwd* ...)))])
                         (format-id stx "~a?" (syntax-e kwd)))
    (syntax/loc stx
      (begin (begin (provide kwd?*) (define (kwd?* v) (symbol-ci=? v 'kwd*))) ...))]))

;; -----------------------------------------------------------------------------

(define (symbol-ci=? s1 s2)
  (and
   (symbol? s1)
   (symbol? s2)
   (string-ci=? (symbol->string s1) (symbol->string s2))))

(define-sql-keyword-predicate
  select
  from
  where
  and)

;; Check for query parameters. Currently only for Postgres.
(define (postgres-parameter? s)
  (and
   (or (string? s) (symbol? s))
   (let ([str (if (string? s) s (symbol->string s))])
     (and
      (= 2 (string-length str))
      (eq? #\$ (string-ref str 0))
      (string->number (string (string-ref str 1)))))))

(module+ test
  (require rackunit rackunit-abbrevs)

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

  (check-apply* postgres-parameter?
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
   [3
    => #f]
  )
)
