#lang typed/racket/base

(provide (all-from-out trivial/regexp))

(require (rename-in trivial/regexp
  [regexp-match: regexp-match]
  [regexp: regexp]
  [pregexp: pregexp]
  [byte-regexp: byte-regexp]
  [byte-pregexp: byte-pregexp]
  [let-regexp: let-regexp]
  [define-regexp: define-regexp]))
