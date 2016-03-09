#lang typed/racket/base

(module+ test
  (require
    trivial/math
    trivial/vector
    typed/rackunit)

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

   (test-case "large vector"
     (let-vector: ([v* (make-vector 200 #f)])
       (check-true (for/and ([v (in-vector (vector-map: not v*))]) v))))

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
   (test-case "vector/length map!"
     (check-equal? (vector-map!: add1 (vector 1)) (vector 2)))

   (test-case "vector/length map! via let"
     (check-equal?
       (let ()
         (: v (Vectorof (Vectorof Integer)))
         (define-vector: v (vector (vector 1) (vector 2 2) (vector 3 3 3) (vector 4 4 4 4)))
         (vector-map!: (lambda ([x : (Vectorof Integer)]) (vector (vector-length: x)))  v))
       '#(#(1) #(2) #(3) #(4))))

   (test-case "map!^3"
     (check-equal?
       (vector-map!: add1 (vector-map!: add1 (vector-map!: add1 (vector 0 0 0))))
       (vector 3 3 3)))

   (test-case "plain map!"
     (check-equal?
       ((lambda ([v : (Vectorof (Vectorof Any))])
         (vector-map!: (lambda ([x : (Vectorof Any)]) (vector (vector-ref: x 0)))  v))
        (vector (vector 1) (vector 2 2) (vector 3 3 3) (vector 4 4 4 4)))
       '#(#(1) #(2) #(3) #(4))))

   (test-case "large vector"
     (let-vector: ([v* (ann (make-vector 200 #f) (Vectorof Boolean))])
       (vector-map!: not v*)
       (check-true (for/and ([v (in-vector v*)]) v))))

   (test-case "higher-order map! pass"
     (check-equal?
       ((lambda ([f : (-> (-> Symbol Symbol) (Vectorof Symbol) (Vectorof Symbol))])
         (f (lambda (x) 'hi) (vector 'x 'yy 'z)))
        vector-map!:)
       (vector 'hi 'hi 'hi)))

   (test-case "higher-order map! fail"
     (check-exn exn:fail:contract?
       (lambda ()
         ((lambda ([f : (-> (-> Integer Integer) (Vectorof Integer) (Vectorof Integer))])
           (vector-ref: (f add1 (vector 0 0)) 3))
          vector-map!:))))
  ;)

  ;(test-suite "vector-append:"
    (test-case "append"
      (let-vector: ([v (vector 0 0 8)]
                    [v2 (vector 1 2)])
        (check-equal?
          (vector-ref: (vector-append: v2 v) 4)
          8)))
  ;)

  ;(test-suite "vector->list:"
    (test-case "vector->list basic"
      (let-vector: ([v (vector 8 8 8 1 8)])
        (check-equal?
          (vector->list: v)
          '(8 8 8 1 8))))

    (test-case "large vector->list"
      (check-equal?
        (vector->list: (ann (make-vector 300 '()) (Vectorof (Listof Any))))
        (build-list 300 (lambda (i) '()))))
  ;)

  ;(test-suite "vector->immutable-vector"
    (test-case "vector->immutable, basic"
      (check-equal?
        (vector-ref: (vector->immutable-vector: (vector 'a 'd 'e)) 0)
        'a))
    (test-case "vector->immutable"
      (check-equal?
        (vector-ref: (vector->immutable-vector: (vector 9 9 4)) 0)
        9))
  ;)

  ;(test-suite "vector-fill!"
    (test-case "vfill basic"
      (let-vector: ([v (vector 2 3 1)])
        (check-equal? (vector-fill!: v 9) (void))
        (check-equal? (vector-ref v 2) 9)))
  ;)

  ;(test-suite "vector-take"
    (test-case "take basis"
      (let-vector: ([v (vector 2 3 1)])
        (check-equal? (vector-take: v 3) v)
        (check-equal? (vector-take: v 2) (vector 2 3))
        (check-equal? (vector-take: v 1) (vector 2))
        (check-equal? (vector-take: v 0) (vector))))
  ;)

  ;(test-suite "vector-take-right"
    (test-case "take-right basic"
      (let-vector: ([v (vector 2 3 1)])
        (check-equal? (vector-take-right: v 3) v)
        (check-equal? (vector-take-right: v 2) (vector 3 1))
        (check-equal? (vector-take-right: v 1) (vector 1))
        (check-equal? (vector-take-right: v 0) (vector))))
  ;)

  ;(test-suite "vector-drop-right"
    (test-case "drop-right basic"
      (let-vector: ([v (vector 2 3 1)])
        (check-equal? (vector-drop-right: v 0) v)
        (check-equal? (vector-drop-right: v 1) (vector 2 3))
        (check-equal? (vector-drop-right: v 2) (vector 2))
        (check-equal? (vector-drop-right: v 3) (vector))))
  ;)

  ;(test-suite "vector-drop"
    (test-case "drop basic"
      (let-vector: ([v (vector 2 3 1)])
        (check-equal? (vector-drop: v 0) v)
        (check-equal? (vector-drop: v 1) (vector 3 1))
        (check-equal? (vector-drop: v 2) (vector 1))
        (check-equal? (vector-drop: v 3) (vector))))
  ;)

  ;; -- define-vector:
  (let ()
    (define-vector: v (vector 1 1 2 2))
    (check-equal? (vector-ref: v 0) 1))
  ;; -- let-vector:
)

;; -----------------------------------------------------------------------------
;  ;; -- parse-vector-length
;  ;; --- '#
;  (check-equal?
;    (parse-vector-length #'#())
;    0)
;;  (check-equal?
;;    (parse-vector-length #`#,'#(1 2))
;;    2)
;  (check-equal?
;    (parse-vector-length #'#(1 2 3 4))
;    4)
;  (check-equal?
;    (parse-vector-length #'#(a b c e s aue d al))
;    8)
;  ;; --- vector
;  (check-equal?
;    (parse-vector-length #'(vector))
;    0)
;  (check-equal?
;    (parse-vector-length #'(vector 0 1))
;    2)
;  ;; --- make-vector
;  (check-equal?
;    (parse-vector-length #'(make-vector -1 1))
;    #f)
;  (check-equal?
;    (parse-vector-length #'(make-vector 0 8))
;    0)
;  (check-equal?
;    (parse-vector-length #'(make-vector 3 3))
;    3)
;  (check-equal?
;    (parse-vector-length #'(make-vector 99))
;    99)
;  ;; --- build-vector
;  (check-equal?
;    ;; Type error
;    (parse-vector-length #'(build-vector -1))
;    #f)
;  (check-equal?
;    (parse-vector-length #'(build-vector 0 (lambda (x) x)))
;    0)
;  (check-equal?
;    (parse-vector-length #'(build-vector 3 (lambda (x) 8)))
;    3)
;  (check-equal?
;    (parse-vector-length #'(build-vector 61 add1))
;    61)
