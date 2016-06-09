#lang racket/base

;; TODO can't retrive values from define:number

(provide
  (for-syntax set-trivial-print)
  (rename-out
    [define: define]
    [let: let]
    ;;
    [format: format]
    [printf: printf]
    [+: +]
    [-: -]
    [*: *]
    [/: /]
    [quotient: quotient]
    [expt: expt]
    ;;
    [pair?: pair?]
    [null?: null?]
    [cons: cons]
    [car: car]
    [cdr: cdr]
    [list?: list?]
    [length: length]
    [list-ref: list-ref]
    [list-tail: list-tail]
    [append: append]
    [reverse: reverse]
    ;;
    [regexp-match: regexp-match]
    [regexp: regexp]
    [pregexp: pregexp]
    [byte-regexp: byte-regexp]
    [byte-pregexp: byte-pregexp]
    ;;
    [vector-length: vector-length]
    [vector-ref: vector-ref]
    [vector-set!: vector-set!]
    [vector-map: vector-map]
    [vector-map!: vector-map!]
    [vector-append: vector-append]
    [vector->list: vector->list]
    [vector->immutable-vector: vector->immutable-vector]
    [vector-fill!: vector-fill!]
    [vector-take: vector-take]
    [vector-take-right: vector-take-right]
    [vector-drop: vector-drop]
    [vector-drop-right: vector-drop-right]
))

(require
  (for-syntax
    trivial/private/common
    racket/base
    (only-in trivial/private/parameters set-trivial-print))
  trivial/untyped/format
  trivial/untyped/list
  trivial/untyped/math
  trivial/untyped/regexp
  trivial/untyped/vector
)

(define-syntax define: (make-keyword-alias 'define
  (lambda (stx)
    (or (format-define stx)
        (num-define stx)
        (lst-define stx)
        (rx-define stx)
        ;(fun-define stx)
        (vec-define stx)))))

(define-syntax let: (make-keyword-alias 'let
  (lambda (stx)
    (or (format-let stx)
        ;(fun-let stx)
        (num-let stx)
        (lst-let stx)
        (rx-let stx)
        (vec-let stx)))))
