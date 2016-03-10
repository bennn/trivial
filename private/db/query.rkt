#lang racket/base

;; Parsing SQL queries


(provide
  query/expand
)

(require
  trivial/private/common
  trivial/private/db/schema
  trivial/private/db/postgres
  (only-in racket/port with-input-from-string)
  (only-in racket/format ~a)
  (only-in racket/string string-replace)
  racket/match
  (for-syntax syntax/parse racket/syntax typed/racket/base)
)

;; =============================================================================

(define (query-parser stx)
  (define str (if (string? (syntax-e stx)) (syntax-e stx) (quoted-stx-value? stx)))
  (and
   (string? str)
   (match (with-input-from-string
            (string-append "(" (sanitize-for-read str) ")")
            read)
    [(list (? select?) sel* ... (? from?) database rest* ...)
     (define flat-sel*
       (if (and (not (null? sel*))
                (list? (car sel*)))
         (if (null? (cdr sel*))
           (car sel*)
           (error 'internal-error "Failed to parse query '~a'" str))
         sel*))
     (define condition* (condition-parser rest*))
     (list flat-sel* database condition*)]
    [_ #f])))

(define (sanitize-for-read str)
  (string-replace
    (string-replace str "," " ")
    "'" "\""))

(define (condition-parser v*)
  (let loop ([v* v*])
    (match v*
     ['()
      '()]
     [(list (or (? where?) (? and?)) db+row '= v rest* ...)
      (cons (cons (~a db+row) (~a v)) ;; ~a is a little confusing
            (loop rest*))]
     [(cons _ rest*)
      (loop rest*)])))

(define-values (query-key query? query-def query-let)
  (make-value-property 'db:query query-parser))
(define-syntax-class/predicate query/expand query?)

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (check-apply* query-parser
   [#'"SELECT a FROM b"
    => '((a) b ())]
   [#'"select * from c"
    => '((*) c ())]
   [#'"select (a, b, c) from d"
    => '((a b c) d ())]
   [#'"select a, b from d limit 10"
    => '((a b) d ())]
   [#'"select a, b from d limit 10 where d.a = \"hello\""
    => '((a b) d (("d.a" . "hello")))]
   [#'"select a, b from d where a = 'hi' and b = 3"
    => '((a b) d (("a" . "hi") ("b" . "3")))]
  )

  (check-apply* sanitize-for-read
   ["hello"
    => "hello"]
   ["what, the, 'heck'"
    => "what  the  \"heck\""]
  )

  (check-apply* condition-parser
   ['()
    => '()]
   ['(limit something = anotherthing)
    => '()]
   ['(limit something where a = b and y = zzz)
    => '(("a" . "b") ("y" . "zzz"))]
  )
)
