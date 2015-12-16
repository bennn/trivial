#lang typed/racket/base

(provide (all-from-out trivial/format))

(require (rename-in trivial/format
  [format: format]
  [printf: printf]))
