#lang racket/base
(require (for-syntax racket/base syntax/parse))

(define-for-syntax DEBUG #f)

(define log-file (open-output-file "log.txt" #:exists 'append))

(define-syntax (log stx)
  (syntax-parse stx
   [(_ message)
    (unless (string? (syntax->datum (syntax message)))
      (error 'nope))
    (if DEBUG
      #'(displayln (string-append "LOG " message) log-file)
      #'(void))]))


(log "wepa")
;(log 22)

(define-syntax (++ stx)
  (syntax-parse stx
   [(++ s1 s2)
    (syntax-property
      (syntax (string-append s1 s2))
      'mytype 'string)]))

(define-syntax (log1 stx)
  (syntax-parse stx
   [(log1 msg)
    (define e (local-expand #'msg 'expression '()))
    (printf "asd ~a\n" e)
    (if (or (string? (syntax->datum #'msg))
            (syntax-property e 'mytype))
      #'"SAFE"
      #'"NOTSAFE")]))

(log1 "yolasdf")
(log1 "adsf")
(log1 (++ "asdf" "cde"))
