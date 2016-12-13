#lang typed/racket/base

(provide
  set!
  (all-from-out racket/list)

  define-list:
  let-list:
  pair?:
  null?:
  cons:
  car:
  cdr:
  list?:
  length:
  list-ref:
  list-tail:
  append:
  reverse:
  ;map:
)

;; -----------------------------------------------------------------------------

(require
  racket/list
  trivial/private/set-bang
  (only-in trivial/private/list
    define-list:
    let-list:
    pair?:
    null?:
    cons:
    car:
    cdr:
    list?:
    length:
    list-ref:
    list-tail:
    append:
    reverse:
    ;map:
))
