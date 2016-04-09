#lang typed/racket/base

;; Constant-folding math operators.
;; Where possible, they simplify their arguments.

(provide
  +: -: *: /:
  ;; Same signature as the racket/base operators,
  ;;  but try to simplify arguments during expansion.

  expt:

  define-num: let-num:

  set!
)

(require
  trivial/private/set-bang
  (only-in trivial/private/math
    +: -: *: /: expt: let-num: define-num:))
