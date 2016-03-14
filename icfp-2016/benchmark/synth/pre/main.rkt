#lang typed/racket/base

(require 
         "typed-data.rkt")

(require "sequencer.rkt")
(require "drum.rkt")

(require "mixer.rkt")

(require "synth.rkt")

(require (for-syntax racket/base syntax/parse) racket/stxparam)

(begin-for-syntax
 (define-syntax-class mixand
   #:attributes (signal weight)
   (pattern [signal:expr (~datum #:weight) weight:expr])
   (pattern signal:expr #:with weight #'1)))

(define-syntax (mix/sugar stx)
  (syntax-parse stx
    [(_ sig:mixand ...)
     #'(mix (list sig.signal sig.weight) ...)]))

;; Test from Vincent's repo.
(: large-test (-> (Vectorof Integer)))
(define (large-test)
 (emit
  (mix/sugar
   (sequence 2 (list
     (note 'C 5 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 1)
     (note 'A# 4 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 3)
     (note 'G 4 1)
     (cons #f 3)
     (note 'G 4 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 1)
     (note 'F 5 1)
     (cons #f 1)
     (note 'E 5 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 9))
     380 sawtooth-wave)
   (drum 8 '(O #f #f #f X #f #f #f) 380))))

;; Small test, for development
(: small-test (-> (Vectorof Integer)))
(define (small-test)
  (emit
   (mix/sugar
    (sequence 1 (list
      (note 'C 5 1)
      (cons #f 1)
      (note 'C 5 1))
      1200 sawtooth-wave)
    (drum 1 '(O #f #f #f X) 1200))))

(define (mid-test)
  (emit
   (mix/sugar
    (sequence 1 (list
      (note 'D 0 1)
      (note 'D 5 1)
      (cons #f 1)
      (note 'D 3 1)
      (note 'D 8 1)
      (cons #f 1)
      (note 'D 5 1)
      (note 'D 10 1)
      (cons #f 1)
      (note 'D 0 1)
      (note 'D 5 1)
      (cons #f 1)
      (note 'D 3 1)
      (note 'D 8 1)
      (cons #f 1)
      (note 'D 6 1)
      ;; (note 'D 11 1)
      (cons #f 1)
      (note 'D 5 1)
      (note 'D 10 1)
      (cons #f 2)
      (note 'D 0 1)
      (note 'D 5 1)
      (cons #f 1)
      (note 'D 3 1)
      (note 'D 8 1)
      (cons #f 1)
      (note 'D 5 1)
      (note 'D 10 1)
      (cons #f 1)
      (note 'D 3 1)
      (note 'D 8 1)
      (cons #f 1)
      (note 'D 0 1)
      (note 'D 5 1))
              1200 sawtooth-wave)
    (drum 1 '(O #f X #f O #f X #f O #f X #f O O X X) 360))))

(: main (-> Void))
(define (main)
  ;; (mid-test) ;; 37ms
  (large-test) ;; 110ms
  ;; (small-test) ;; 3ms
  (void))

(time (main))
