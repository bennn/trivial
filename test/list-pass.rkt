#lang typed/racket/base

(module+ test
  (require
    trivial/math
    trivial/list
    typed/rackunit)

  ;; -- pair?
  (check-equal?
    (ann (pair?: '()) #f)
    #f)
  (check-equal?
    (ann (pair?: '(1 2)) #t)
    #t)

  ;; -- null?
  (check-equal?
    (ann (null?: '()) #t)
    #t)
  (check-equal?
    (ann (null?: '(1 2)) #f)
    #f)

  ;; -- cons
  (check-equal?
    (ann (cons: 1 '(2)) (List Natural Natural))
    '(1 2))
  (check-equal?
    (ann (-: (length: (cons: 1 (cons: 2 (cons: 3 '())))) 3) Zero)
    0)

  ;; -- car
  (check-equal?
    (ann (car: '(a b)) 'a)
    'a)

  ;; -- cdr
  (check-equal?
    (cdr: '(1 2))
    '(2))

  ;; -- list?
  (check-equal?
    (ann (list?: '()) #t)
    #t)
  (check-equal?
    (ann (list?: '(1 2 3)) #t)
    #t)
  (check-equal?
    (list?: 'A)
    #f)

  ;; -- length:
  (check-equal?
    (length: '()) 0)
  (check-equal?
    (length: (list 1 2 2)) 3)
  (check-equal?
    (ann (-: (length: (list 5 5 5 5 5)) 4)
         One)
    1)
  (let-list: ([v1 (list 2 3 4)]
                [v2 (list 4 3 2)])
    (check-equal?
      (ann (+: 1 (-: (*: 5 (length: v1)) (+: (*: 4 3) (length: v2))))
           One)
      1))
  (let ()
    (define-list: v1 (list 2 3 4))
    (define-list: v2 (list 4 3 2))
    (check-equal?
      (ann (*: 5 (-: (length: v1) (*: 1 1 (length: v2) 1)))
           Zero)
      0))

  ;(test-suite "list-ref:"
    (test-case "list/length ref"
      (check-equal? (list-ref: (list 1) 0) 1))

    (test-case "list/length ref, via let"
      (let-list: ([v (list 2)])
        (check-equal? (list-ref: v 0) 2)))

    (test-case "list/length ref, via define"
      (define-list: v (list "a" "bee" "sea"))
      (check-equal? (list-ref: v 2) "sea"))

    (test-case "plain list ref"
      (check-equal?
        ((lambda (v) (list-ref: v 3)) (list 8 2 19 3 0))
        3))

    (test-case "higher-order list ref"
      (check-exn exn:fail:contract?
        (lambda ()
          ((lambda ([f : (-> (Listof Any) Natural Any)])
            (f (list 0 1 2) 10)) list-ref:))))

    (test-case "2-level ref"
      (let-list: ([v1  (list 'X)])
        (let-list: ([v2 (list v1)])
          (check-equal? (list-ref: (list-ref: v2 0) 0) 'X))))
  ;)

 ;(test-suite "map:"
 ;  (test-case "list/length map"
 ;    (check-equal? (map: add1 (list 1)) (list 2)))

 ;  (test-case "list/length map via let"
 ;    (check-equal?
 ;      (let-list: ([v (list (list 1) (list 2 2)
 ;                               (list 3 3 3) (list 4 4 4 4))])
 ;        (map: (lambda ([x : (Listof Any)]) (length: x)) v)) ;; dammit map
 ;      (list 1 2 3 4)))

 ;  (test-case "map^3"
 ;    (check-equal?
 ;      (map: add1 (map: add1 (map: add1 (list 0 0 0))))
 ;      (list 3 3 3)))

 ;  (test-case "plain map"
 ;    (check-equal?
 ;      ((lambda ([v : (Listof (Listof Any))])
 ;        (map: (lambda ([x : (Listof Any)]) (length: x)) v))
 ;       (list (list 1) (list 2 2) (list 3 3 3) (list 4 4 4 4)))
 ;      (list 1 2 3 4)))

 ;  (test-case "large list"
 ;    (let-list: ([v* (make-list 200 #f)])
 ;      (check-true (for/and ([v (in-list (map: not v*))]) v))))

 ;  (test-case "higher-order map pass"
 ;    (check-equal?
 ;      ((lambda ([f : (-> (-> Symbol String) (Listof Symbol) (Listof String))])
 ;        (f symbol->string '(x yy z)))
 ;       map:)
 ;      (list "x" "yy" "z")))

 ;  (test-case "higher-order map fail"
 ;    (check-exn exn:fail:contract?
 ;      (lambda ()
 ;        ((lambda ([f : (-> (-> Integer Integer) (Listof Integer) (Listof Integer))])
 ;          (list-ref: (f add1 (list 0 0)) 3))
 ;         map:))))
 ;)

  ;(test-suite "append:"
    (test-case "append"
      (let-list: ([v (list 0 0 8)]
                    [v2 (list 1 2)])
        (check-equal?
          (list-ref: (append: v2 v) 4)
          8)))
  ;)

  ;(test-suite "reverse:"
    (test-case "reverse"
      (let-list: ([v (list 0 0 8)]
                  [v2 (list 1 2)])
        (let-list: ([v+ (reverse: (append: v2 v))])
          (check-equal? (car: v+) 8)
          (check-equal? (ann (-: (length: v+) 5) Zero) 0))))
  ;)

)
