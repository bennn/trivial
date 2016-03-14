#lang typed/racket/base

;; Adapter for gregor's `structs.rkt` file.

(provide
  (struct-out YMD)
  (struct-out HMSN)
  Month)

(require
  "../base/types.rkt")

(require "structs.rkt"
)
