#lang typed/racket/base

(module+ test
  (require
    trivial
    typed/rackunit)

  (test-case "cons"
    (check-equal?
      (ann (cons 1 '(2)) (List Natural Natural))
      '(1 2))
    (check-equal?
      (ann (- (length (cons 1 (cons 2 (cons 3 '())))) 3) Zero)
      0))

  (test-case "car"
    (define (id-list (x : (Listof Symbol))) : (Listof Symbol) x)
    (check-equal? (car '(a b)) 'a)
    (check-equal? (car (id-list '(a b))) 'a))

  (test-case "cdr"
    (define (id-list (x : (Listof Symbol))) : (Listof Symbol) x)
    (check-equal? (cdr '(1 2)) '(2))
    (check-equal? (cdr (id-list '(a b))) '(b))
  )

  (test-case "length"
    (check-equal?
      (length '()) 0)
    (check-equal?
      (length (list 1 2 2)) 3)
    (check-equal?
      (ann (-  (length (list 5 5 5 5 5)) 4)
           One)
      1)
    (let ([v1 (list 2 3 4)]
          [v2 (list 4 3 2)])
      (check-equal?
        (ann (+ 1 (- (* 5 (length v1)) (+ (* 4 3) (length v2))))
             One)
        1))
    (let ()
      (define v1 (list 2 3 4))
      (define v2 (list 4 3 2))
      (check-equal?
        (ann (* 5 (- (length v1) (* 1 1 (length v2) 1)))
             Zero)
        0)))

  (test-case "list-ref"
    (check-equal? (list-ref (list 1) 0) 1)

    (let ([v (list 2)])
      (check-equal? (list-ref v 0) 2))

    (let ()
      (define v (list "a" "bee" "sea"))
      (check-equal? (list-ref v 2) "sea"))

    (check-equal?
      ((lambda (v) (list-ref v 3)) (list 8 2 19 3 0))
      3)

    (check-exn exn:fail:contract?
      (lambda ()
        ((lambda ([f : (-> (Listof Any) Natural Any)])
          (f (list 0 1 2) 10)) list-ref)))

    (check-equal?
      (ann (list-ref (list-ref '((A)) 0) 0) 'A)
      'A)

    (let ([v1  (list 'X)])
      (let ([v2 (list v1)])
        (check-equal? (list-ref (list-ref v2 0) 0) 'X))))

  (test-case "map"
    (check-equal? (map add1 (list 1)) (list 2))

    (check-equal?
      (let ([v (list (list 1) (list 2 2)
                               (list 3 3 3) (list 4 4 4 4))])
        (map (lambda ([x : (Listof Any)]) (length x)) v)) ;; dammit map
      (list 1 2 3 4))

    (check-equal?
      (map add1 (map add1 (map add1 (list 0 0 0))))
      (list 3 3 3))

    (check-equal?
      ((lambda ([v : (Listof (Listof Any))])
        (map (lambda ([x : (Listof Any)]) (length x)) v))
       (list (list 1) (list 2 2) (list 3 3 3) (list 4 4 4 4)))
      (list 1 2 3 4))

    (let ([v* (make-list 200 #f)])
      (check-true (for/and ([v (in-list (map not v*))]) v)))

    (check-equal?
      ((lambda ([f : (-> (-> Symbol String) (Listof Symbol) (Listof String))])
        (f symbol->string '(x yy z)))
       map)
      (list "x" "yy" "z"))

    (check-exn exn:fail:contract?
      (lambda ()
        ((lambda ([f : (-> (-> Integer Integer) (Listof Integer) (Listof Integer))])
          (list-ref (f add1 (list 0 0)) 3))
         map)))

    (check-equal?
      (map (lambda ([x : Natural]) (add1 x)) '(8 2 1 3))
      '(9 3 2 4))

    (check-equal?
      (map (λ ([x : String] [y : String])
              (string-append x y))
            '("hello")
            '("world"))
      '("helloworld"))

    (check-equal?
      (map (λ ([x : String] [y : String])
              (format "~a ~a" x y))
            '("hello")
            '("world"))
      '("hello world"))

    (check-equal?
      (map (lambda ([x : Integer] [y : Integer] [z : Integer])
              (+ (* x y) z))
        '(1 2 3)
        '(4 5 6)
        '(8 9 10))
    '(12 19 28)))

  (test-case "append"
    (let ([v (list 0 0 8)]
                  [v2 (list 1 2)])
      (check-equal?
        (list-ref (append v2 v) 4)
        8)))

  (test-case "reverse"
    (let ([v (list 0 0 8)]
                [v2 (list 1 2)])
      (let ([v+ (reverse (append v2 v))])
        (check-equal? (car v+) 8)
        (check-equal? (ann (- (length v+) 5) Zero) 0))))

  (test-case "sort"
    (let ([v (list 3 1 2)])
      (let ([v+ (sort v <)])
        (check-equal? (ann (- (length v+) 2) One) 1))))

)
