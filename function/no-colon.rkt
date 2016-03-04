#lang typed/racket/base

(provide (all-from-out trivial/function))

(require (rename-in trivial/function
 [curry: curry]))
