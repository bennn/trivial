#lang typed/racket/base

;; Provides the same bindings as `trivial/main`,
;;  but without the trailing colon.

(provide (all-from-out trivial))

(require
  (rename-in trivial
   ;; -- from `format.rkt`
   [format: format]
   [printf: printf]

   ;; -- from `regexp.rkt`
   [regexp-match: regexp-match]
   [regexp: regexp]
   [pregexp: pregexp]
   [byte-regexp: byte-regexp]
   [byte-pregexp: byte-pregexp]
   [define-regexp: define-regexp]
   [define-pregexp: define-pregexp]
   [define-byte-regexp: define-byte-regexp]
   [define-byte-pregexp: define-byte-pregexp]

   ;; -- from `math.rkt`
   [+: +]
   [-: -]
   [*: *]
   [/: /]
))
