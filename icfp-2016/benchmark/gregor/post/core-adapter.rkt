#lang typed/racket/base
(require trivial/no-colon)

;; Adapter for gregor's `structs.rkt` file.

(provide
  (struct-out YMD)
  (struct-out HMSN)
  Month)

(require
  "../base/types.rkt")

(require "core-structs.rkt"
)
