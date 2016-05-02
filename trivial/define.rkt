#lang typed/racket/base

(provide
  define: let:
  (all-from-out trivial/private/set-bang)
)

(require
  trivial/private/set-bang
  (for-syntax
    trivial/private/common
    syntax/parse
    typed/racket/base)
  (only-in trivial/private/format
    format-define
    format-let)
  (only-in trivial/private/list
    lst-define
    lst-let)
  (only-in trivial/private/math
    num-define
    num-let)
  (only-in trivial/private/regexp
    rx-define
    rx-let)
  (only-in trivial/private/function
    fun-define
    fun-let)
  (only-in trivial/private/vector
    vec-define
    vec-let))

(define-syntax define: (make-keyword-alias 'define
  (lambda (stx)
    (or (format-define stx)
        (num-define stx)
        (lst-define stx)
        (rx-define stx)
        (fun-define stx)
        (vec-define stx)))))

(define-syntax let: (make-keyword-alias 'let
  (lambda (stx)
    (or (format-let stx)
        (fun-let stx)
        (num-let stx)
        (lst-let stx)
        (rx-let stx)
        (vec-let stx)))))
