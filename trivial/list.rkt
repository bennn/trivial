#lang racket/base

(provide
  cons
  make-list
  build-list
  car
  cdr
  length
  list
  list-ref
  list-tail
  append
  reverse
  map
  sort
  #;andmap
  #;ormap
  #;for-each
  #;foldl
  #;foldr
  #;filter
  #;remove
  #;remq
  #;remv
  #;remove*
  #;remq*
  #;remv*
  #;member
)

;; -----------------------------------------------------------------------------

(require
  trivial/private/list)
