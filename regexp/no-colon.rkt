#lang typed/racket/base

(provide (all-from-out trivial/regexp))

(require (rename-in trivial/regexp
  [regexp-match: regexp-match]
  [regexp: regexp]
  [pregexp: pregexp]
  [byte-regexp: byte-regexp]
  [byte-pregexp: byte-pregexp]
  [let-regexp: let-regexp]
  [let-pregexp: let-pregexp]
  [let-byte-regexp: let-byte-regexp]
  [let-byte-pregexp: let-byte-pregexp]
  [define-regexp: define-regexp]
  [define-pregexp: define-pregexp]
  [define-byte-regexp: define-byte-regexp]
  [define-byte-pregexp: define-byte-pregexp]))
