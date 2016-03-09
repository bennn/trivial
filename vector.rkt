#lang typed/racket/base

(provide
  (all-from-out racket/vector)

  define-vector:
  let-vector:
  vector-length:
  vector-ref:
  vector-set!:
  vector-map:
  vector-map!:
  vector-append:
  vector->list:
  vector->immutable-vector:
  vector-fill!:
  vector-take:
  vector-take-right:
  vector-drop:
  vector-drop-right:
;  vector-split-at:
;  vector-split-at-right:
)

;; -----------------------------------------------------------------------------

(require
  racket/vector
  (only-in trivial/private/vector
    define-vector:
    let-vector:
    vector-length:
    vector-ref:
    vector-set!:
    vector-map:
    vector-map!:
    vector-append:
    vector->list:
    vector->immutable-vector:
    vector-fill!:
    vector-take:
    vector-take-right:
    vector-drop:
    vector-drop-right:
))
