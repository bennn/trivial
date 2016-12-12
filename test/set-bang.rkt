#lang racket/base

;; TODO the `vector-ref` should not be optimized
;; -- its variable is set!
;; -- the set! eventually makes an unsafe reference

;; ideas:
;; - don't produce unsafe-ref, produce a different checked'ref
;;   that's easier for the compiler to optimize
;; - don't produce unsafe-ref at all
;; - disallow set!
;; - globally analyze, find all set! vars before macro expanding
;; - leave a disclaimer
;; - undo relevant changes
;; - detect if interpolant used in optimization

;; - possible to do a whole-program 1st pass?

#;
(module+ test
  (require (except-in trivial add1) rackunit)

  (check-equal?
    (let* ([len 2]
           [v (build-vector len values)]
           [i 0])
      (let loop ()
        (if (<= i len)
          (+ (vector-ref v i)
             (begin
               (set! i (add1 i))
               (loop)))
          0)))
    1) ;; should not segfault!
)
