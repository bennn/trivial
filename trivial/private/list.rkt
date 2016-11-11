#lang typed/racket/base

(provide
  (for-syntax L-dom)
  (rename-out
    [-make-list make-list]
    [-build-list build-list]
    [-cons cons]
    [-car car]
    [-cdr cdr]
    [-list list]
    [-length length]
    [-list-ref list-ref]
    [-list-tail list-tail]
    [-append append]
    [-reverse reverse]
    [-map map]
    [-sort sort]
    #;[-andmap andmap]
    #;[-ormap ormap]
    #;[-for-each for-each]
    #;[-foldl foldl]
    #;[-foldr foldr]
    #;[-filter filter]
    #;[-remove remove]
    #;[-remq remq]
    #;[-remv remv]
    #;[-remove* remove*]
    #;[-remq* remq*]
    #;[-remv* remv*]
    #;[-member member])
)

;; -----------------------------------------------------------------------------

(module typed-list typed/racket
  (require
    typed/racket/unsafe
    (only-in racket/unsafe/ops
      unsafe-car
      unsafe-cdr))

  ;; Thank you based Asumu
  (unsafe-require/typed racket/unsafe/ops
    (unsafe-cons-list (All (A B)
     (-> A B (Pairof A B))))
    (unsafe-list-ref (All (A B)
     (-> (Listof A) B A)))
    (unsafe-list-tail (All (A B C)
     (-> (Pairof A B) C B))))

  (unsafe-provide
    unsafe-cons-list unsafe-list-ref unsafe-list-tail)

  (provide
    unsafe-car unsafe-cdr null
    make-list build-list
    cons
    car
    cdr
    length
    list
    list-ref
    list-tail
    append
    reverse
    map
    sort)
)

;; -----------------------------------------------------------------------------

(require
  (rename-in trivial/private/define [let +let])
  (only-in racket/unsafe/ops
    unsafe-vector-set!
    unsafe-vector-ref)
  (prefix-in tr- 'typed-list)
  ;(only-in typed/racket λ: : Integer)
  racket/list
  trivial/private/function
  trivial/private/integer
  (for-syntax
    typed/untyped-utils
    syntax/parse
    racket/base
    racket/syntax
    trivial/private/common))

;; =============================================================================

(define-for-syntax L-dom
  (make-abstract-domain L #:leq <=
    [(~or '(e* ...)
           (e* ...))
     (length (syntax-e #'(e* ...)))]))

(define-for-syntax (bounds-error sym v i)
  (raise-user-error sym "[~a:~a] Index '~a' out of range for list '~a'"
    (syntax-line v)
    (syntax-column v)
    i
    (syntax->datum v)))

;; -----------------------------------------------------------------------------

(define-syntax (-make-list stx)
  (with-syntax ([+make-list (if (syntax-local-typed-context?) (syntax/loc stx tr-make-list) (syntax/loc stx  make-list))])
    (syntax-parse stx
     [(_ e:~> . e*)
      (define n (φ-ref (φ #'e.~>) I-dom))
      (cond
       [(integer? n)
        (log-ttt-infer+ 'make-list stx)
        (⊢ (syntax/loc stx
             (+make-list e.~> . e*))
           (φ-set (φ-init) L-dom n))]
       [else
        (log-ttt-infer- 'make-list stx)
        (syntax/loc stx
          (+make-list . e*))])]
     [(_ . e*)
      (syntax/loc stx
        (+make-list . e*))]
     [_:id
      (syntax/loc stx
        +make-list)])))

(define-syntax (-build-list stx)
  (with-syntax ([+build-list (if (syntax-local-typed-context?) (syntax/loc stx tr-build-list) (syntax/loc stx  build-list))])
    (syntax-parse stx
     [(_ e:~> f:~>)
      (define n (φ-ref (φ #'e.~>) I-dom))
      (define arr
        (let ([arr (φ-ref (φ #'f.~>) A-dom)])
          (if (⊤? A-dom arr)
            (raise-user-error 'build-list (⊤-msg arr))
            arr)))
      (cond
       [(and (integer? arr) (not (= 1 arr)))
        (raise-user-error 'build-list (format-arity-error stx 1))]
       [(integer? n)
        (log-ttt-infer+ 'build-list stx)
        (⊢ (syntax/loc stx
             (+build-list e.~> f.~>))
           (φ-set (φ-init) L-dom n))]
       [else
        (log-ttt-infer- 'build-list stx)
        (syntax/loc stx
          (+build-list e.~> f.~>))])]
     [(_ . e*)
      (syntax/loc stx
        (+build-list . e*))]
     [_:id
      (syntax/loc stx
        +build-list)])))

(define-syntax (-cons stx)
  (with-syntax ([(+unsafe-cons-list +cons)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx (tr-unsafe-cons-list tr-cons))
                   (syntax/loc stx (tr-unsafe-cons-list tr-cons)))])
    (syntax-parse stx
     [(_ e1 e2:~>)
      (define n (φ-ref (φ #'e2.~>) L-dom))
      (cond
       [(integer? n)
        (log-ttt-infer+ 'cons stx)
        (⊢ (syntax/loc stx
             (+unsafe-cons-list e1 e2.~>))
           (φ-set (φ-init) L-dom (+ n 1)))]
       [else
        (log-ttt-infer- 'cons stx)
        (syntax/loc stx
          (+cons e1 e2.~>))])]
     [(_ . e*)
      (syntax/loc stx
        (+cons . e*))]
     [_:id
      (syntax/loc stx
        +cons)])))

(define-syntax (-list stx)
  (with-syntax ([+list (if (syntax-local-typed-context?) (syntax/loc stx tr-list) (syntax/loc stx list))])
    (syntax-parse stx
     [(_ . e*)
      (log-ttt-infer+ 'list stx)
      (⊢ (syntax/loc stx
           (+list . e*))
         (φ-set (φ-init) L-dom (length (syntax-e #'e*))))]
     [_:id
      (syntax/loc stx +list)])))

(define-syntax (-null stx)
  (with-syntax ([+null (if (syntax-local-typed-context?) (syntax/loc stx tr-null) (syntax/loc stx null))])
    (syntax-parse stx
     [_:id
      (log-ttt-infer+ 'null stx)
      (⊢ (syntax/loc stx +null)
         (φ-set (φ-init) L-dom 0))])))

(define-syntax (-car stx)
  (with-syntax ([(+car +unsafe-car)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx
                     (tr-car tr-unsafe-car))
                   (syntax/loc stx
                     (car unsafe-car)))])
    (syntax-parse stx
     [(_ e:~>)
      (define n (φ-ref (φ #'e.~>) L-dom))
      (cond
       [(⊥? L-dom n)
        (log-ttt-check- 'car stx)
        (syntax/loc stx
          (+car e.~>))]
       [(⊤? L-dom n)
        (raise-user-error 'car (⊤-msg n))]
       [(positive? n)
        (log-ttt-check+ 'car stx)
        (syntax/loc stx
          (+unsafe-car e.~>))]
       [else
        (bounds-error 'car (syntax/loc stx e.~>) n)])]
     [(_ . e*)
      (syntax/loc stx
        (+car . e*))]
     [_:id
      (syntax/loc stx
        +car)])))

(define-syntax (-cdr stx)
  (with-syntax ([(+cdr +unsafe-cdr)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx
                     (tr-cdr tr-unsafe-cdr))
                   (syntax/loc stx
                     (cdr unsafe-cdr)))])
    (syntax-parse stx
     [(_ e:~>)
      (define n (φ-ref (φ #'e.~>) L-dom))
      (cond
       [(⊥? L-dom n)
        (log-ttt-check- 'cdr stx)
        (syntax/loc stx
          (+cdr e.~>))]
       [(⊤? L-dom n)
        (raise-user-error 'cdr (⊤-msg n))]
       [(positive? n)
        (log-ttt-check+ 'cdr stx)
        (syntax/loc stx
          (+unsafe-cdr e.~>))]
       [else
        (bounds-error 'cdr (syntax/loc stx e.~>) n)])]
     [(_ . e*)
      (syntax/loc stx
        (+cdr . e*))]
     [_:id
      (syntax/loc stx
        +cdr)])))

(define-syntax (-length stx)
  (with-syntax ([+length (if (syntax-local-typed-context?) (syntax/loc stx tr-length) (syntax/loc stx length))])
    (syntax-parse stx
     [(_ e:~>)
      (define n (φ-ref (φ #'e.~>) L-dom))
      (cond
       [(⊥? L-dom n)
        (log-ttt-check- 'length stx)
        (syntax/loc stx
          (+length e.~>))]
       [(⊤? L-dom n)
        (raise-user-error 'length (⊤-msg n))]
       [else
        (log-ttt-check+ 'length stx)
        (⊢ (quasisyntax/loc stx
             '#,n)
           (φ-set (φ-init) I-dom n))])]
     [(_ . e*)
      (syntax/loc stx
        (+length . e*))]
     [_:id
      (syntax/loc stx
        +length)])))

(define-syntax (-list-ref stx)
  (with-syntax ([(+list-ref +unsafe-list-ref)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx
                     (tr-list-ref tr-unsafe-list-ref))
                   (syntax/loc stx
                     (list-ref unsafe-list-ref)))])
    (syntax-parse stx
     [(_ e1:~> e2:~>)
      (define n (φ-ref (φ #'e1.~>) L-dom))
      (define i (φ-ref (φ #'e2.~>) I-dom))
      (cond
       [(or (⊥? L-dom n) (⊥? I-dom i))
        (log-ttt-check- 'list-ref stx)
        (syntax/loc stx
          (+list-ref e1.~> e2.~>))]
       [(⊤? L-dom n)
        (raise-user-error 'list-ref (⊤-msg n))]
       [(⊤? L-dom i)
        (raise-user-error 'list-ref (⊤-msg i))]
       [(and (<= 0 i) (< i n))
        (log-ttt-check+ 'list-ref stx)
        (syntax/loc stx
          (+unsafe-list-ref e1.~> e2.~>))]
       [else
        (bounds-error 'list-ref (syntax/loc stx e1.~>) i)])]
     [(_ . e*)
      (syntax/loc stx
        (+list-ref . e*))]
     [_:id
      (syntax/loc stx
        +list-ref)])))

(define-syntax (-list-tail stx)
  (with-syntax ([(+list-tail +unsafe-list-tail)
                 (if (syntax-local-typed-context?)
                   (syntax/loc stx
                     (tr-list-tail tr-unsafe-list-tail))
                   (syntax/loc stx
                     (list-tail unsafe-list-tail)))])
    (syntax-parse stx
     [(_ e1 e2)
      (define n (φ-ref (φ #'e1.~>) L-dom))
      (define i (φ-ref (φ #'e2.~>) I-dom))
      (cond
       [(or (⊥? L-dom n) (⊥? I-dom i))
        (log-ttt-check- 'list-tail stx)
        (syntax/loc stx
          (+list-tail e1.~> e2.~>))]
       [(⊤? L-dom n)
        (raise-user-error 'list-tail (⊤-msg n))]
       [(⊤? I-dom i)
        (raise-user-error 'list-tail (⊤-msg i))]
       [(and (<= 0 i) (< i n))
        (log-ttt-check+ 'list-tail stx)
        (syntax/loc stx
          (+unsafe-list-tail e1.~> e2.~>))]
       [else
        (bounds-error 'list-tail stx)])]
     [(_ . e*)
      (syntax/loc stx
        (+list-tail . e*))]
     [_:id
      (syntax/loc stx
        +list-tail)])))

(define-syntax (-append stx)
  (with-syntax ([+append (if (syntax-local-typed-context?) (syntax/loc stx tr-append) (syntax/loc stx append))])
    (syntax-parse stx
     [(_ e*:~> ...)
      (define n* (for/list ([e~ (in-list (syntax-e #'(e*.~> ...)))])
                   (φ-ref (φ e~) L-dom)))
      (define sum-n (reduce L-dom + 0 n*))
      (cond
       [(⊤? L-dom sum-n)
        (raise-user-error 'append (⊤-msg sum-n))]
       [else
        (if (⊥? L-dom sum-n)
          (log-ttt-check- 'append stx)
          (log-ttt-check+ 'append stx))
        (⊢ (syntax/loc stx
             (+append e*.~> ...))
           (φ-set (φ-init) L-dom (for/sum ([n (in-list n*)]) n)))])]
     [_:id
      (syntax/loc stx
        +append)])))

(define-syntax (-reverse stx)
  (with-syntax ([+reverse (if (syntax-local-typed-context?) (syntax/loc stx tr-reverse) (syntax/loc stx reverse))])
    (syntax-parse stx
     [(_ e:~>)
      (define n (φ-ref (φ #'e.~>) L-dom))
      (cond
       [(⊥? L-dom n)
        (log-ttt-infer- 'reverse stx)
        (syntax/loc stx
          (+reverse e.~>))]
       [(⊤? L-dom n)
        (raise-user-error 'reverse (⊤-msg n))]
       [else
        (⊢ (syntax/loc stx
             (+reverse e.~>))
           (φ-set (φ-init) L-dom n))])]
     [(_ . e*)
      (syntax/loc stx
        (+reverse . e*))]
     [_:id
      (syntax/loc stx
        +reverse)])))

(define-syntax (-map stx)
  (with-syntax ([+map (if (syntax-local-typed-context?) (syntax/loc stx tr-map) (syntax/loc stx map))])
    (syntax-parse stx
     [(_ f:~> e*:~> ...)
      (define arr
        (let ([arr (φ-ref (φ #'f.~>) A-dom)])
          (if (⊤? A-dom arr)
            (raise-user-error 'map (⊤-msg arr))
            arr)))
      (define n* (for/list ([e (in-list (syntax-e #'(e*.~> ...)))])
                   (define n (φ-ref (φ e) L-dom))
                   (if (⊤? L-dom n)
                     (raise-user-error 'map (⊤-msg n))
                     n)))
      (define num-lists (length n*))
      (define min-length (⊓* L-dom n*))
      (cond
       [(and (integer? arr) (not (= arr num-lists)))
        (raise-user-error 'map (format-arity-error stx num-lists))]
       [else
        (if (⊥? L-dom min-length)
          (log-ttt-check- 'map stx)
          (log-ttt-check+ 'map stx))
        (⊢ (syntax/loc stx
             (+map f.~> e*.~> ...))
           (φ-set (φ-init) L-dom min-length))])]
     [(_ . e*)
      (syntax/loc stx
        (+map . e*))]
     [_:id
      (syntax/loc stx
        +map)])))

(define-syntax (-sort stx)
  (with-syntax ([+sort (if (syntax-local-typed-context?) (syntax/loc stx tr-sort) (syntax/loc stx sort))])
    (syntax-parse stx
     [(_ e:~> . e*)
      (define n (φ-ref (φ #'e.~>) L-dom))
      (cond
       [(⊥? L-dom n)
        (log-ttt-infer- 'sort stx)
        (syntax/loc stx
          (+sort e.~> . e*))]
       [(⊤? L-dom n)
        (raise-user-error 'sort (⊤-msg n))]
       [else
        (⊢ (syntax/loc stx
             (+sort e.~> . e*))
           (φ-set (φ-init) L-dom n))])]
     [(_ . e*)
      (syntax/loc stx
        (+sort . e*))]
     [_:id
      (syntax/loc stx
        +sort)])))

