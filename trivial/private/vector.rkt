#lang typed/racket/base

(provide
  define-vector:
  let-vector:
  vector-length:
  vector-ref:
  vector-set!:
  vector-map:
  vector-map!:
  vector-append:
  vector->list:
  vector->immutable-vector:
  vector-fill!:
  vector-take:
  vector-take-right:
  vector-drop:
  vector-drop-right:
;  vector-split-at:
;  vector-split-at-right:

  ;; --- private
  (for-syntax
    vec-define
    vec-let
    parse-vector-length
    vector-length-key)
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/unsafe/ops
    unsafe-vector-set!
    unsafe-vector-ref)
  trivial/private/math
  racket/vector
  (for-syntax
    trivial/private/common
    trivial/private/sequence
    typed/racket/base
    syntax/parse))

;; =============================================================================

(begin-for-syntax
  (define (parse-vector-length stx)
    (syntax-parse stx #:literals (#%plain-app vector make-vector build-vector)
     [(~or '#(e* ...)
           #(e* ...)
           ;; TODO #{} #[] #6{} ...
           (#%plain-app vector e* ...)
           (#%plain-app vector e* ...)
           (vector e* ...))
      (length (syntax-e #'(e* ...)))]
     [(~or (make-vector n e* ...)
           (#%plain-app make-vector n e* ...)
           (build-vector n e* ...)
           (#%plain-app build-vector n e* ...))
      #:with n-stx (stx->num #'n)
      #:when (syntax-e #'n-stx)
      (syntax-e #'n-stx)]
     [_ #f]))

  (define-values (vector-length-key vec? vec-define vec-let)
    (make-value-property 'vector:length parse-vector-length))
  (define-syntax-class/predicate vector/length vec?)
)

;; -----------------------------------------------------------------------------

(define-syntax define-vector: (make-keyword-alias 'define vec-define))
(define-syntax let-vector: (make-keyword-alias 'let vec-let))

(define-syntax vector-length: (make-alias #'vector-length
  (lambda (stx) (syntax-parse stx
   [(_ v:vector/length)
    (syntax/loc stx 'v.evidence)]
   [_ #f]))))

(define-syntax vector-ref: (make-alias #'vector-ref
  (lambda (stx) (syntax-parse stx
   [(_ v:vector/length e)
    #:with i-stx (stx->num #'e)
    #:when (syntax-e #'i-stx)
    (let ([i (syntax-e #'i-stx)])
      (unless (< i (syntax-e #'v.evidence))
        (bounds-error 'vector-ref: #'v i))
      (syntax/loc stx (unsafe-vector-ref v.expanded 'i-stx)))]
   [_ #f]))))

(define-syntax vector-set!: (make-alias #'vector-set!
  (lambda (stx) (syntax-parse stx
   [(_ v:vector/length e val)
    #:with i-stx (stx->num #'e)
    #:when (syntax-e #'i-stx)
    (let ([i (syntax-e #'i-stx)])
      (unless (< i (syntax-e #'v.evidence))
        (bounds-error 'vector-set!: #'v i))
      (syntax/loc stx (unsafe-vector-set! v.expanded 'i-stx val)))]
   [_ #f]))))

(define-syntax vector-map: (make-alias #'vector-map
  (lambda (stx) (syntax-parse stx
   [(_ f v:vector/length)
    #:with f+ (gensym 'f)
    #:with v+ (gensym 'v)
    #:with v++ (syntax-property
                 (if (small-sequence-size? (syntax-e #'v.evidence))
                   (with-syntax ([(i* ...) (for/list ([i (in-range (syntax-e #'v.evidence))]) i)])
                     (syntax/loc stx
                       (let ([f+ f] [v+ v.expanded])
                         (vector (f+ (unsafe-vector-ref v+ 'i*)) ...))))
                   (syntax/loc stx
                     (let ([f+ f] [v+ v.expanded])
                       (build-vector 'v.evidence (lambda ([i : Integer])
                                                 (f+ (vector-ref: v+ i)))))))
                 vector-length-key
                 (syntax-e #'v.evidence))
    (syntax/loc stx v++)]
   [_ #f]))))

(define-syntax vector-map!: (make-alias #'vector-map!
  (lambda (stx) (syntax-parse stx
   [(_ f v:vector/length)
    #:with f+ (gensym 'f)
    #:with v+ (gensym 'v)
    #:with v++ (syntax-property
                 #'(let ([f+ f]
                         [v+ v.expanded])
                     (for ([i (in-range 'v.evidence)])
                       (unsafe-vector-set! v+ i (f+ (unsafe-vector-ref v+ i))))
                     v+)
                 vector-length-key
                 (syntax-e #'v.evidence))
    (syntax/loc stx v++)]
   [_ #f]))))

(define-syntax vector-append: (make-alias #'vector-append
  (lambda (stx) (syntax-parse stx
   [(_ v1:vector/length v2:vector/length)
    #:with v1+ (gensym 'v1)
    #:with v2+ (gensym 'v2)
    (define l1 (syntax-e #'v1.evidence))
    (define l2 (syntax-e #'v2.evidence))
    (syntax-property
      (if (and (small-sequence-size? l1)
               (small-sequence-size? l2))
        (with-syntax ([(i1* ...) (for/list ([i (in-range l1)]) i)]
                      [(i2* ...) (for/list ([i (in-range l2)]) i)])
          (syntax/loc stx
            (let ([v1+ v1.expanded]
                  [v2+ v2.expanded])
              (vector (vector-ref: v1+ i1*) ...
                      (vector-ref: v2+ i2*) ...))))
        (quasisyntax/loc stx
          (let ([v1+ v1.expanded]
                [v2+ v2.expanded])
            (build-vector
              #,(+ l1 l2)
              (lambda (i)
                (if (< i '#,l1)
                  (unsafe-vector-ref v1+ i)
                  (unsafe-vector-ref v2+ i)))))))
      vector-length-key
      (+ l1 l2))]
   [_ #f]))))

(define-syntax vector->list: (make-alias #'vector->list
  (lambda (stx) (syntax-parse stx
   [(_ v:vector/length)
    #:with v+ (gensym 'v)
    (define len (syntax-e #'v.evidence))
    (if (small-sequence-size? len)
      (with-syntax ([(i* ...) (for/list ([i (in-range len)]) i)])
        (syntax/loc stx
          (let ([v+ v.expanded])
            (list (unsafe-vector-ref v+ i*) ...))))
      (syntax/loc stx
        (let ([v+ v.expanded])
          (build-list 'v.evidence (lambda (i) (unsafe-vector-ref v+ i))))))]
   [_ #f]))))

(define-syntax vector->immutable-vector: (make-alias #'vector->immutable-vector
  (lambda (stx) (syntax-parse stx
   [(_ v:vector/length)
    (syntax-property
      (syntax/loc stx (vector->immutable-vector v.expanded))
      vector-length-key
      (syntax-e #'v.evidence))]
   [_ #f]))))

(define-syntax vector-fill!: (make-alias #'vector-fill!
  (lambda (stx) (syntax-parse stx
   [(_ v:vector/length val)
    #:with v+ (gensym 'v)
    (define len (syntax-e #'v.evidence))
    (syntax-property
      (syntax/loc stx
        (let ([v+ v.expanded])
          (for ([i (in-range 'v.evidence)])
            (unsafe-vector-set! v+ i val))))
      vector-length-key
      (syntax-e #'v.evidence))]
   [_ #f]))))

(begin-for-syntax (define-syntax-rule (make-slice-op left? take?)
  (lambda (stx)
    (syntax-parse stx
     [(op-name v:vector/length n)
      #:with n-stx (stx->num #'n)
      #:when (exact-nonnegative-integer? (syntax-e #'n-stx))
      #:with (lo hi)
        (if 'take?
          (if 'left?
            (list 0 (syntax-e #'n-stx))
            (list
              (- (syntax-e #'v.evidence) (syntax-e #'n-stx))
              (syntax-e #'v.evidence)))
          (if 'left?
            (list (syntax-e #'n-stx) (syntax-e #'v.evidence))
            (list 0 (- (syntax-e #'v.evidence) (syntax-e #'n-stx)))))
      #:with n+ (gensym 'n)
      #:with v+ (gensym 'v)
      (unless (<= (syntax-e #'n-stx) (syntax-e #'v.evidence))
        (bounds-error (syntax-e #'op-name) #'v
          (if 'take? (if 'left? (syntax-e #'hi) (syntax-e #'lo))
                     (if 'left? (syntax-e #'lo) (syntax-e #'hi)))))
      (syntax-property
        (syntax/loc stx
          (let ([v+ v.expanded]
                [n+ (-: 'hi 'lo)])
            (build-vector n+ (lambda ([i : Integer]) (unsafe-vector-ref v+ (+: i 'lo))))))
        vector-length-key
        (syntax-e #'v.evidence))]
     [(op-name v n:int/expand)
      (bounds-error (syntax-e #'op-name) #'v (stx->num #'n.expanded))]
     [_ #f]))))

(define-syntax vector-take:
  (make-alias #'vector-take       (make-slice-op #t #t)))

(define-syntax vector-take-right:
  (make-alias #'vector-take-right (make-slice-op #f #t)))

(define-syntax vector-drop-right:
  (make-alias #'vector-drop-right (make-slice-op #f #f)))

(define-syntax vector-drop:
  (make-alias #'vector-drop       (make-slice-op #t #f)))


