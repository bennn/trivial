#lang typed/racket/base

(module+ test
  (require
    trivial/integer
    trivial/vector
    trivial/define
    trivial/integer
    typed/rackunit)

  (test-case "vector-length"
    (check-equal?
      (vector-length '#()) 0)
    (check-equal?
      (vector-length (vector 1 2 2)) 3)
    (check-equal?
      (ann (- (vector-length (vector 5 5 5 5 5)) 4)
           One)
      1)
    (let ([v1 (vector 2 3 4)]
                  [v2 (vector 4 3 2)])
      (check-equal?
        (ann (+ 1 (- (* 5 (vector-length v1)) (+ (* 4 3) (vector-length v2))))
             One)
        1))
    (let ()
      (define v1 (vector 2 3 4))
      (define v2 (vector 4 3 2))
      (check-equal?
        (ann (* 5 (- (vector-length v1) (* 1 1 (vector-length v2) 1)))
             Zero)
        0))

    (check-equal? (vector-ref (vector 1) 0) 1)

    (let ([v (vector 2)])
      (check-equal? (vector-ref v 0) 2))

    (let ()
      (define v (vector "a" "bee" "sea"))
      (check-equal? (vector-ref v 2) "sea"))

    (check-equal?
      ((lambda (v) (vector-ref v 3)) (vector 8 2 19 3 0))
      3)

    (check-exn exn:fail:contract?
      (lambda ()
        ((lambda ([f : (-> (Vectorof Any) Natural Any)])
          (f (vector 0 1 2) 10)) vector-ref)))

    (let ([v1  (vector 'X)])
      (let ([v2 (vector v1)])
        (check-equal? (vector-ref (vector-ref v2 0) 0) 'X))))

  (test-case "vector-set"
    (check-equal? (vector-set! (vector 1) 0 8) (void))

    (let ([v (vector 2)])
      (vector-set! v 0 3)
      (check-equal? (vector-ref v 0) 3))

    (let ()
      (define v (vector "a" "bee" "sea"))
      (vector-set! v 1 "bye")
      (check-equal? (vector-ref v 1) "bye"))

    (check-equal?
      ((lambda (v) (vector-set! v 3 4) (vector-ref v 3)) (vector 8 2 19 3 0))
      4)

    (check-exn exn:fail:contract?
      (lambda ()
        ((lambda ([f : (-> (Vectorof Any) Natural Any Void)])
          (f (vector 0 1 2) 10 9)) vector-set!))))

  (test-case "vector-map"
    (check-equal? (vector-map add1 (vector 1)) (vector 2))

    (check-equal?
      (let ([v : (Vectorof (Vectorof Integer))
               (vector (vector 1) (vector 2 2)
                       (vector 3 3 3) (vector 4 4 4 4))])
        (vector-map vector-length v))
      (vector 1 2 3 4))

    (check-equal?
      (vector-map add1 (vector-map add1 (vector-map add1 (vector 0 0 0))))
      (vector 3 3 3))

    (check-equal?
      ((lambda ([v : (Vectorof (Vectorof Any))])
        (vector-map vector-length v))
       (vector (vector 1) (vector 2 2) (vector 3 3 3) (vector 4 4 4 4)))
      (vector 1 2 3 4))

    (let ([v* (make-vector 200 #f)])
      (check-true (for/and ([v (in-vector (vector-map not v*))]) v)))

    (check-equal?
      ((lambda ([f : (-> (-> Symbol String) (Vectorof Symbol) (Vectorof String))])
        (f symbol->string '#(x yy z)))
       vector-map)
      (vector "x" "yy" "z"))

    (check-exn exn:fail:contract?
      (lambda ()
        ((lambda ([f : (-> (-> Integer Integer) (Vectorof Integer) (Vectorof Integer))])
          (vector-ref (f add1 (vector 0 0)) 3))
         vector-map))))

  (test-case "vector-map!"
     (check-equal? (vector-map! add1 (vector 1)) (vector 2))

     (check-equal?
       (let ()
         (: v (Vectorof (Vectorof Integer)))
         (define v (vector (vector 1) (vector 2 2) (vector 3 3 3) (vector 4 4 4 4)))
         (vector-map! (lambda ([x : (Vectorof Integer)]) (vector (vector-length x)))  v))
       '#(#(1) #(2) #(3) #(4)))

     (check-equal?
       (vector-map! add1 (vector-map! add1 (vector-map! add1 (vector 0 0 0))))
       (vector 3 3 3))

     (check-equal?
       ((lambda ([v : (Vectorof (Vectorof Any))])
         (vector-map! (lambda ([x : (Vectorof Any)]) (vector (vector-ref x 0)))  v))
        (vector (vector 1) (vector 2 2) (vector 3 3 3) (vector 4 4 4 4)))
       '#(#(1) #(2) #(3) #(4)))

     (let ([v* (ann (make-vector 200 #f) (Vectorof Boolean))])
       (vector-map! not v*)
       (check-true (for/and ([v (in-vector v*)]) v)))

     (check-equal?
       ((lambda ([f : (-> (-> Symbol Symbol) (Vectorof Symbol) (Vectorof Symbol))])
         (f (lambda (x) 'hi) (vector 'x 'yy 'z)))
        vector-map!)
       (vector 'hi 'hi 'hi))

     (check-exn exn:fail:contract?
       (lambda ()
         ((lambda ([f : (-> (-> Integer Integer) (Vectorof Integer) (Vectorof Integer))])
           (vector-ref (f add1 (vector 0 0)) 3))
          vector-map!))))

  (test-case "vector-append"
   (let ([v (vector 0 0 8)]
         [v2 (vector 1 2)])
    (check-equal?
       (vector-ref (vector-append v2 v) 4)
       8)))

  (test-case "vector->list"
    (let ([v (vector 8 8 8 1 8)])
      (check-equal?
        (vector->list v)
        '(8 8 8 1 8)))

    (check-equal?
      (vector->list (ann (make-vector 300 '()) (Vectorof (Listof Any))))
      (build-list 300 (lambda (i) '()))))

  (test-case "vector->immutable-vector"
    (check-equal?
      (vector-ref (vector->immutable-vector (vector 'a 'd 'e)) 0)
      'a)
    (check-equal?
      (vector-ref (vector->immutable-vector (vector 9 9 4)) 0)
      9))

  (test-case "vector-fill!"
    (let ([v (vector 2 3 1)])
      (check-equal? (vector-fill! v 9) (void))
      (check-equal? (vector-ref v 2) 9)))

  (test-case "take"
    (let ([v (vector 2 3 1)])
      (check-equal? (vector-take v 3) v)
      (check-equal? (vector-take v 2) (vector 2 3))
      (check-equal? (vector-take v 1) (vector 2))
      (check-equal? (vector-take v 0) (vector))))

  (test-case "vector-take-right"
    (let ([v (vector 2 3 1)])
      (check-equal? (vector-take-right v 3) v)
      (check-equal? (vector-take-right v 2) (vector 3 1))
      (check-equal? (vector-take-right v 1) (vector 1))
      (check-equal? (vector-take-right v 0) (vector))))

  (test-case "vector-drop-right"
    (let ([v (vector 2 3 1)])
      (check-equal? (vector-drop-right v 0) v)
      (check-equal? (vector-drop-right v 1) (vector 2 3))
      (check-equal? (vector-drop-right v 2) (vector 2))
      (check-equal? (vector-drop-right v 3) (vector))))

  (test-case "vector-drop"
    (let ([v (vector 2 3 1)])
      (check-equal? (vector-drop v 0) v)
      (check-equal? (vector-drop v 1) (vector 3 1))
      (check-equal? (vector-drop v 2) (vector 1))
      (check-equal? (vector-drop v 3) (vector))))

  (test-case "vector:define"
    (let ()
      (define v (vector 1 1 2 2))
      (check-equal? (vector-ref v 0) 1))
    (let ()
      (define v (vector 2 1 3))
      (define w (vector 2 1 3))
      (check-equal? (vector-length v) 3)
      (check-equal? (vector-length w) 3)
      (check-equal?
        ((lambda ([z : (Vectorof Integer)])
          (vector-length z)) v)
        3)))

;; -----------------------------------------------------------------------------

  (test-case "vector-length:more"
    (check-equal?
      (ann (vector-length #()) Zero)
      0)
    (check-equal?
      (ann (- (vector-length '#(1 2)) 2) Zero)
      0)
    (check-equal?
      (ann (- (vector-length '#(1 2 3 4)) 4) Zero)
      0)
    (check-equal?
      (ann (- (vector-length #(a b c e s aue d al)) 8) Zero)
      0)
    ;; --- vector
    (check-equal?
      (ann (vector-length (vector)) Zero)
      0)
    (check-equal?
      (ann (- (vector-length (vector 0 1)) 2) Zero)
      0)
    ;; --- make-vector
    (check-equal?
      (ann (vector-length (make-vector 0 8)) Zero)
      0)
    (check-equal?
      (ann (- (vector-length (make-vector 3 3)) 3) Zero)
      0)
    (check-equal?
      (ann (- (vector-length (make-vector 99)) 99) Zero)
      0)
    ;; --- build-vector
    (check-equal?
      (ann (vector-length (build-vector 0 (lambda (x) x))) Zero)
      0)
    (check-equal?
      (ann (- (vector-length (build-vector 3 (lambda (x) 8))) 3) Zero)
      0)
    (check-equal?
      (ann (- (vector-length (build-vector 61 add1)) 61) Zero)
      0))

  (test-case "nested:vector-ref"
    (check-equal? ;; b/c vector-length is the only way to cash in
      (ann (vector-length (vector-ref (vector (vector) (vector)) 0))
           Zero)
      0)
    (check-equal? ;; b/c vector-length is the only way to cash in
      (ann (- (vector-ref (vector-append (vector 0 1) (vector 3 4)) 3) 4)
           Zero)
      0))
  ;; TODO more tests like this
)
