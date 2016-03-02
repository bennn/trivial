#lang typed/racket/base

(module+ test
  (require
    trivial/math
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

  ;; -- vector-length:
  (check-equal?
    (vector-length: '#()) 0)
  (check-equal?
    (vector-length: (vector 1 2 2)) 3)
  (check-equal?
    (ann (-: (vector-length: (vector 5 5 5 5 5)) 4)
         One)
    1)
  (let-vector: ([v1 (vector 2 3 4)]
                [v2 (vector 4 3 2)])
    (check-equal?
      (ann (+: 1 (-: (*: 5 (vector-length: v1)) (+: (*: 4 3) (vector-length: v2))))
           One)
      1))
  (let ()
    (define-vector: v1 (vector 2 3 4))
    (define-vector: v2 (vector 4 3 2))
    (check-equal?
      (ann (*: 5 (-: (vector-length: v1) (*: 1 1 (vector-length: v2) 1)))
           Zero)
      0))

  ;; -- vector-ref
  (test-case "vector/length ref"
    (check-equal? (vector-ref: (vector 1) 0) 1))

  (test-case "vector/length ref, via let"
    (let-vector: ([v (vector 2)])
      (check-equal? (vector-ref: v 0) 2)))

  (test-case "vector/length ref, via define"
    (define-vector: v (vector "a" "bee" "sea"))
    (check-equal? (vector-ref: v 2) "sea"))

  (test-case "plain vector ref"
    (check-equal?
      ((lambda (v) (vector-ref: v 3)) (vector 8 2 19 3 0))
      3))

  (test-case "higher-order vector ref"
    (check-exn exn:fail:contract?
      (lambda ()
        ((lambda ([f : (-> (Vectorof Any) Natural Any)])
          (f (vector 0 1 2) 10)) vector-ref:))))

  ;; -- define-vector:
  (let ()
    (define-vector: v (vector 1 1 2 2))
    (check-equal? (vector-ref: v 0) 1))
  ;; -- let-vector:
)
