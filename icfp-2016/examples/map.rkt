#lang typed/racket/base
(require trivial/no-colon)
(define (cite (p : String) (d : String))
    (printf "~a v. ~a, U.S.\n" p d))
(define plaintiff*
  '("Mistretta" "Rasul" "Chisholm"))
(define defendant*
  '("U.S." "Bush" "Georgia"))
(void (map cite plaintiff* defendant*))
