#lang typed/racket/base
(require
  trivial/vector
  trivial/math
  racket/vector
  plot/typed/no-gui
  math/statistics)

(define NUM-TRIALS 8)

(define-vector: v1 (make-vector (expt: 10 1)))
(define-vector: v2 (make-vector (expt: 10 2)))
(define-vector: v3 (make-vector (expt: 10 3)))
(define-vector: v4 (make-vector (expt: 10 4)))
(define-vector: v5 (make-vector (expt: 10 5)))
(define-vector: v6 (make-vector (expt: 10 6)))
(define-vector: v7 (make-vector (expt: 10 7)))
;(define-vector: v8 (make-vector (expt: 10 8)))
;(define-vector: v9 (make-vector (expt: 10 9)))

(define-syntax-rule (tt e)
 (let ([proc (lambda () e)])
  (mean (for/list : (Listof Integer)
                  ([i (in-range NUM-TRIALS)])
     (let-values (((res cpu real gc) (time-apply proc '())))
       cpu)))))

(define before*
  (list
    (tt (vector-map (lambda (x) x) v1))
    (tt (vector-map (lambda (x) x) v2))
    (tt (vector-map (lambda (x) x) v3))
    (tt (vector-map (lambda (x) x) v4))
    (tt (vector-map (lambda (x) x) v5))
    (tt (vector-map (lambda (x) x) v6))
    (tt (vector-map (lambda (x) x) v7))
    #;(tt (vector-map (lambda (x) x) v8))
    #;(tt (vector-map (lambda (x) x) v9))))

(define after*
  (list
    (tt (vector-map: (lambda (x) x) v1))
    (tt (vector-map: (lambda (x) x) v2))
    (tt (vector-map: (lambda (x) x) v3))
    (tt (vector-map: (lambda (x) x) v4))
    (tt (vector-map: (lambda (x) x) v5))
    (tt (vector-map: (lambda (x) x) v6))
    (tt (vector-map: (lambda (x) x) v7))
    #;(tt (vector-map: (lambda (x) x) v8))
    #;(tt (vector-map: (lambda (x) x) v9))))

;; TODO plot-pict
(plot-file
 (list
  (lines (for/list : (Listof (List Integer Real))
                   ([i (in-naturals 1)]
                    [t (in-list before*)])
           (list i t))
         #:color 1
         #:label "before")
  (lines (for/list : (Listof (List Integer Real))
                   ([i (in-naturals 1)]
                    [t (in-list after*)])
           (list i t))
         #:color 2
         #:label "after"))
  "map-microbench.png"
  'png)
