#lang racket/base

;; TODO define/let

(provide
  (for-syntax set-trivial-print)
  (rename-out
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
  (for-syntax (only-in trivial/private/parameters set-trivial-print))
  trivial/untyped/format
  trivial/untyped/list
  trivial/untyped/math
  trivial/untyped/regexp
  trivial/untyped/vector
)
