#lang typed/racket/base

(module+ test
  (require
    trivial/vector
    typed/rackunit)

  (require/typed (for-template trivial/vector)
    (parse-vector-length (-> Syntax (Option Natural))))

  ;; -- parse-vector-length
  ;; --- '#
  (check-equal?
    (parse-vector-length #'#())
    0)
;  (check-equal?
;    (parse-vector-length #`#,'#(1 2))
;    2)
  (check-equal?
    (parse-vector-length #'#(1 2 3 4))
    4)
  (check-equal?
    (parse-vector-length #'#(a b c e s aue d al))
    8)
  ;; --- vector
  (check-equal?
    (parse-vector-length #'(vector))
    0)
  (check-equal?
    (parse-vector-length #'(vector 0 1))
    2)
  ;; --- make-vector
  (check-equal?
    (parse-vector-length #'(make-vector -1 1))
    #f)
  (check-equal?
    (parse-vector-length #'(make-vector 0 8))
    0)
  (check-equal?
    (parse-vector-length #'(make-vector 3 3))
    3)
  (check-equal?
    (parse-vector-length #'(make-vector 99))
    99)
  ;; --- build-vector
  (check-equal?
    ;; Type error
    (parse-vector-length #'(build-vector -1))
    #f)
  (check-equal?
    (parse-vector-length #'(build-vector 0 (lambda (x) x)))
    0)
  (check-equal?
    (parse-vector-length #'(build-vector 3 (lambda (x) 8)))
    3)
  (check-equal?
    (parse-vector-length #'(build-vector 61 add1))
    61)
)
