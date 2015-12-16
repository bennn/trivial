#lang typed/racket/base

;; Provides the same bindings as `trivial/main`,
;;  but without the trailing colon.

(provide
  (all-from-out trivial/format/no-colon)
  (all-from-out trivial/math/no-colon)
  (all-from-out trivial/regexp/no-colon))

(require
  trivial/format/no-colon
  trivial/math/no-colon
  trivial/regexp/no-colon)
