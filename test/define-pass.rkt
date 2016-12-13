#lang typed/racket/base
(require
  trivial/define
  trivial/integer
  trivial/list
  trivial/regexp
  trivial/vector)

(module+ test
  (require typed/rackunit typed/racket/class)

  (check-equal?
    (let ()
      (define n 3)
      (let ([m n])
        (ann (- m n) Zero)))
    0)

  (check-equal?
    (let ([x (regexp "(a*)(b*)")])
      (let ([m (regexp-match x "aaabbb")])
        (if m (string-append (cadr m) (caddr m)) "")))
    "aaabbb")

  (check-equal?
    (let ([v '#(3 9 2)])
      (ann (- (vector-length v) 3) Zero))
    0)

  (check-equal?
    (let ([f (lambda ([x : String] [y : Integer])
                (format "hello(~a) and ~b" x y))])
      (let ([xs '("hi" "hi" "HI")]
             [ys '(4 3 1)])
      (map f xs ys)))
    '("hello(hi) and 100" "hello(hi) and 11" "hello(HI) and 1"))

  ;; Should be okay with "Indiana-style" defines
  (let ()
    (define fact : (-> Integer Integer)
      (lambda (n)
        (if (< n 2) 1 (* n (fact (- n 1))))))
    (check-equal? (fact 5) 120))

  ;; Also with classes
  (let ()
    (define f% : (Rec t (Class (yolo (-> (Instance t)))))
      (class object%
        (super-new)
        (define/public (yolo)
          (new f%))))
    (check-false (not (new f%))))

  ;; let*
  (check-equal?
   (let* ([v (list 1 2 3)]
          [w v]
          [k 42])
     (ann (length w) 3))
   3)

  ;; let with different kinds of bindings
  (check-equal?
   (let ([v (list 1 2 3)]
         [k 42])
     (ann (length v) 3))
   3)
)
