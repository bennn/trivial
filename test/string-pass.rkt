#lang typed/racket/base

(module+ test

  (require
    trivial/regexp
    trivial/string
    trivial/define
    trivial/integer
    typed/rackunit)

  (test-case "string"
    (check-equal?
      (ann (string-length "") Zero)
      0)
    (check-equal?
      (ann (- (string-length "racket") 6) Zero)
      0)
    (check-equal?
      (ann (string-ref "racket" 0) Char)
      #\r)
    #;(check-equal?
      (let ([s (string-append "r" "acket")])
        (check-equal? (string-set! s 0 #\R) (void))
        0
        #;(ann (- (string-length s) 6) Zero))
      0)
    (check-equal?
      (ann (- (string-length (substring "racket" 5)) 1) Zero)
      0)
    (check-equal?
      (ann (- (string-length (substring "racket" 0 3)) 3) Zero)
      0)
    (check-equal?
      (ann (- (string-length (substring "racket" 3 6)) 3) Zero)
      0)
    (check-equal?
      (ann (- (string-length (string-append "rac" "ke" "t")) 6) Zero)
      0))

  (test-case "bytes"
    (check-equal?
      (ann (- (bytes-length #"aaaa") 4) Zero)
      0)
    (check-equal?
      (ann (bytes-ref #"racket" 2) Byte)
      (char->integer #\c))
    #;(check-equal?
      (ann (- (bytes-length (bytes-set! (bytes-append #"" #"racket") 5 #\T)) 6) Zero)
      0)
    (check-equal?
      (ann (- (bytes-length (subbytes #"racket" 2)) 4) Zero)
      0)
    (check-equal?
      (ann (- (bytes-length (subbytes #"racket" 4 6)) 2) Zero)
      0)
    (check-equal?
      (ann (- (bytes-length (bytes-append #"r" #"a" #"ck" #"et")) 6) Zero)
      0))

  (test-case "regexp:string"
    (let ([s0 "a*"]
          [s1 "b*"])
      (check-equal?
        (ann
          (regexp-match (string-append "(" s0 ")" s1) "aaab")
          (U #f (List String String)))
        '("aaab" "aaa"))))

  (test-case "regexp:bytes"
    (let ([s0 #"a*"]
          [s1 #"b*"])
      (check-equal?
        (ann
          (regexp-match (bytes-append #"(" s0 #")" s1) #"aaab")
          (U #f (List Bytes Bytes)))
        '(#"aaab" #"aaa"))))
)
