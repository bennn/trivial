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

  ;(test-suite "vector-ref:"
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

    (test-case "2-level ref"
      (let-vector: ([v1  (vector 'X)])
        (let-vector: ([v2 (vector v1)])
          (check-equal? (vector-ref: (vector-ref: v2 0) 0) 'X))))
  ;)

  ;(test-suite "vector-set!:"
    (test-case "vector/length set!"
      (check-equal? (vector-set!: (vector 1) 0 8) (void)))

    (test-case "vector/length set!, via let"
      (let-vector: ([v (vector 2)])
        (vector-set! v 0 3)
        (check-equal? (vector-ref: v 0) 3)))

    (test-case "vector/length set, via define"
      (define-vector: v (vector "a" "bee" "sea"))
      (vector-set! v 1 "bye")
      (check-equal? (vector-ref: v 1) "bye"))

    (test-case "plain vector set"
      (check-equal?
        ((lambda (v) (vector-set!: v 3 4) (vector-ref: v 3)) (vector 8 2 19 3 0))
        4))

    (test-case "higher-order vector set"
      (check-exn exn:fail:contract?
        (lambda ()
          ((lambda ([f : (-> (Vectorof Any) Natural Any Void)])
            (f (vector 0 1 2) 10 9)) vector-set!:))))
 ;)

 ;(test-suite "vector-map:"
   (test-case "vector/length map"
     (check-equal? (vector-map: add1 (vector 1)) (vector 2)))

   (test-case "vector/length map via let"
     (check-equal?
       (let-vector: ([v (vector (vector 1) (vector 2 2)
                                (vector 3 3 3) (vector 4 4 4 4))])
         (vector-map: vector-length: v))
       (vector 1 2 3 4)))

   (test-case "map^3"
     (check-equal?
       (vector-map: add1 (vector-map: add1 (vector-map: add1 (vector 0 0 0))))
       (vector 3 3 3)))

   (test-case "plain map"
     (check-equal?
       ((lambda ([v : (Vectorof (Vectorof Any))])
         (vector-map: vector-length: v))
        (vector (vector 1) (vector 2 2) (vector 3 3 3) (vector 4 4 4 4)))
       (vector 1 2 3 4)))

   (test-case "higher-order map pass"
     (check-equal?
       ((lambda ([f : (-> (-> Symbol String) (Vectorof Symbol) (Vectorof String))])
         (f symbol->string '#(x yy z)))
        vector-map:)
       (vector "x" "yy" "z")))

   (test-case "higher-order map fail"
     (check-exn exn:fail:contract?
       (lambda ()
         ((lambda ([f : (-> (-> Integer Integer) (Vectorof Integer) (Vectorof Integer))])
           (vector-ref: (f add1 (vector 0 0)) 3))
          vector-map:))))
 ;)

  ;(test-suite "vector-map!:"
  ;)

  ;; -- define-vector:
  (let ()
    (define-vector: v (vector 1 1 2 2))
    (check-equal? (vector-ref: v 0) 1))
  ;; -- let-vector:
)
