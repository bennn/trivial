#lang typed/racket/base

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; -----------------------------------------------------------------------------

(require
  (only-in racket/file file->value))

(require "morse-code-strings.rkt"

 "levenshtein.rkt")

(define word-frequency-list "frequency.rktd")

(define-predicate freq-list? (Listof (List String Integer)))

(: file->words (-> String (Listof String)))
(define (file->words filename)
  (define words+freqs (file->value (string->path filename)))
  (unless (freq-list? words+freqs) (error "expected a frequency list"))
  (for/list : (Listof String) ([word+freq : (List String Integer) words+freqs])
    (car word+freq)))

(: words-small (Listof String))
(define words-small (file->words word-frequency-list))

(: main (-> (Listof String) Void))
(define (main words)
  (for* ([w1 (in-list words)]
         [w2 (in-list words)])
    (string->morse w1)
    (string->morse w2)
    (string-levenshtein w1 w2)
    (string-levenshtein w2 w1)
    (void)))

(time (main words-small)) ;; 200ms
