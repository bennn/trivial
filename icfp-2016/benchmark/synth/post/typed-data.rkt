#lang typed/racket/base
(require trivial/no-colon)

(provide
  Indexes
  In-Indexes
  Weighted-Signal
  Drum-Symbol
  Pattern
  (struct-out Array)
  (struct-out Settable-Array)
  (struct-out Mutable-Array))

(require "data.rkt"
)
(define-type Indexes (Vectorof Integer))
(define-type In-Indexes Indexes)

;; From mix: A Weighted-Signal is a (List (Array Float) Real)
(define-type Weighted-Signal (List Array Real))

;; drum patterns are simply lists with either O (bass drum), X (snare) or
;; #f (pause)
(define-type Drum-Symbol (U 'O 'X #f))
(define-type Pattern (Listof Drum-Symbol))
