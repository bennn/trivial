#lang typed/racket/base

(require
 (only-in racket/file file->lines file->string))

(require "lcs.rkt")

(define LARGE_TEST "prufock.txt")

;; LCS on all pairs of lines in a file
(: main (-> String Void))
(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

(time (main LARGE_TEST))
