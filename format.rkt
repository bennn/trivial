#lang typed/racket/base

;; Statically-checked format strings

(provide
  format:
  ;; (-> (x#:String) Any *N Void)
  ;; Takes 1 required string argument and N additional arguments,
  ;;  where N is the number of format sequences in the string.
  ;;
  ;; If the string is a literal, raises a compile-time arity error if
  ;;  the given number of arguments does not match the format string.
  ;;
  ;; If the string is not a literal, arity-checking happens at runtime.

  printf:
  ;; (-> (x:String) Any *N Void)
  ;; Similar to `format`, but displays the formatted string to `current-output-port`.
)

(require
  (only-in trivial/private/format
    format:
    printf:))
