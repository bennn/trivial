#lang typed/racket/base
(provide (rename-out
  (define-list:            define-list)
  (let-list:               let-list)
  (pair?:                  pair?)
  (null?:                  null?)
  (cons:                   cons)
  (car:                    car)
  (cdr:                    cdr)
  (list?:                  list?)
  (length:                 length)
  (list-ref:               list-ref)
  (list-tail:              list-tail)
  (append:                 append)
  (reverse:                reverse)
  ;(map:                    map)
))
(require trivial/list)

