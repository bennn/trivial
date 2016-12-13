#lang typed/racket/base

;; Stronger types for regular expression matching.

(provide
  set!
  regexp:       define-regexp:       let-regexp:
  pregexp:
  byte-regexp:
  byte-pregexp:
  ;; Expression and definition forms that try checking their argument patterns.
  ;; If check succeeds, will remember the number of pattern groups
  ;; for calls to `regexp-match:`.

  regexp-match:
  ;; (-> Pattern String Any * (U #f (List String *N+1)))
  ;; Match the regular expression pattern against a string.
  ;; If the pattern is determined statically, result will be either #f
  ;;  or a list of N+1 strings, where N is the number of groups specified
  ;;  the pattern.
  ;; Will raise a compile-time exception if the pattern contains unmatched groups.
)

(require
  trivial/private/regexp
  trivial/private/set-bang
)
