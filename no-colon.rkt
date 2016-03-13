#lang typed/racket/base

;; Provides the same bindings as `trivial/main`,
;;  but without the trailing colon.

(provide
  set!
  (all-from-out trivial/define/no-colon)
  (all-from-out trivial/format/no-colon)
  (all-from-out trivial/function/no-colon)
  (all-from-out trivial/math/no-colon)
  (all-from-out trivial/regexp/no-colon)
  (all-from-out trivial/vector/no-colon))

(require
  trivial/private/set-bang
  trivial/define/no-colon
  trivial/format/no-colon
  trivial/function/no-colon
  trivial/math/no-colon
  trivial/regexp/no-colon
  trivial/vector/no-colon)
