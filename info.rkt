#lang info
(define collection "dependent")
(define deps '("base"))
(define build-deps '("cover" "cover-coveralls" "scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "Strongly-typed macros")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/dependent.scrbl")))
