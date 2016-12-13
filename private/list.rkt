#lang typed/racket/base

(provide
  define-list:
  let-list:
  pair?:
  null?:
  cons:
  car:
  cdr:
  list?:
  length:
  list-ref:
  list-tail:
  append:
  reverse:
  ;map:
  ;andmap:
  ;ormap:
  ;for-each:
  ;foldl:
  ;foldr:
  ;filter:
  ;remove:
  ;remq:
  ;remv:
  ;remove*:
  ;remq*:
  ;remv*:
  ;sort:
  ;member:

  ;; --- private
  (for-syntax
    lst-define
    lst-let
    parse-list-length
    lst-length-key ;; TODO generic "data structure length" key?
  )
)

;; -----------------------------------------------------------------------------

(require
  trivial/private/math
  typed/racket/unsafe
  racket/list
  (for-syntax
    trivial/private/common
    trivial/private/sequence
    typed/racket/base
    syntax/parse))

;; Thank you based Asumu
(unsafe-require/typed racket/unsafe/ops
  (unsafe-car (All (A B)
   (case->
     (-> (Listof A) A)
     (-> (Pairof A B) A))))
  (unsafe-cdr (All (A B)
   (-> (Pairof A B) B)))
  (unsafe-cons-list (All (A B)
   (-> A B (Pairof A B))))
  (unsafe-list-ref (All (A B)
   (-> (Listof A) B A)))
  (unsafe-list-tail (All (A B C)
   (-> (Pairof A B) C B)))
)

;; =============================================================================

(begin-for-syntax
  (define (parse-list-length stx)
    (syntax-parse stx #:literals (#%plain-app cons list list* make-list build-list null)
     [(~or '(e* ...)
            (list e* ...)
            (#%plain-app list e* ...))
      (length (syntax-e #'(e* ...)))]
     [(~or (make-list n e* ...)
           (#%plain-app make-list n e* ...)
           (build-list n e* ...)
           (#%plain-app build-list n e* ...))
      #:with n-stx (stx->num #'n)
      #:when (syntax-e #'n-stx)
      (syntax-e #'n-stx)]
     [(~or (cons e es)
           (#%plain-app cons e es))
      #:with n+ (parse-list-length #'es)
      #:when (syntax-e #'n+)
      (+ 1 (syntax-e #'n+))]
     [(~or (list* e* ... es)
           (#%plain-app list* e* ... es))
      #:with n+ (parse-list-length #'es)
      #:when (syntax-e #'n+)
      (+ (length (syntax-e #'(e* ...))) (syntax-e #'n+))]
     [(~or (null)
           (#%plain-app null))
      0]
     [_ #f]))

  (define-values (lst-length-key lst? lst-define lst-let)
    (make-value-property 'list:length parse-list-length))
  (define-syntax-class/predicate list/length lst?)
)

;; -----------------------------------------------------------------------------

(define-syntax define-list: (make-keyword-alias 'define lst-define))
(define-syntax let-list: (make-keyword-alias 'let lst-let))

(define-syntax pair?: (make-alias #'pair?
  (lambda (stx) (syntax-parse stx
   [(_ l:list/length)
    (quasisyntax/loc stx '#,(not (zero? (syntax-e #'l.evidence))))]
   [_ #f]))))

(define-syntax null?: (make-alias #'null?
  (lambda (stx) (syntax-parse stx
   [(_ l:list/length)
    (quasisyntax/loc stx '#,(zero? (syntax-e #'l.evidence)))]
   [_ #f]))))

(define-syntax cons: (make-alias #'cons
  (lambda (stx) (syntax-parse stx
   [(_ x l:list/length)
    #:with l+ (syntax-property
                (syntax/loc stx (unsafe-cons-list x l.expanded))
                lst-length-key (+ 1 (syntax-e #'l.evidence)))
    (syntax/loc stx l+)]
   [_ #f]))))

(define-syntax car: (make-alias #'car
  (lambda (stx) (syntax-parse stx
   [(_ l:list/length)
    (when (zero? (syntax-e #'l.evidence))
      (bounds-error 'car: #'l 0))
    (syntax/loc stx (unsafe-car l.expanded))]
   [_ #f]))))

(define-syntax cdr: (make-alias #'cdr
  (lambda (stx) (syntax-parse stx
   [(_ l:list/length)
    (when (zero? (syntax-e #'l.evidence))
      (bounds-error 'cdr: #'l 0))
    (syntax/loc stx (unsafe-cdr l.expanded))]
   [_ #f]))))

(define-syntax list?: (make-alias #'list?
  (lambda (stx) (syntax-parse stx
   [(_ l:list/length)
    (syntax/loc stx '#t)]
   [_ #f]))))

(define-syntax length: (make-alias #'length
  (lambda (stx) (syntax-parse stx
   [(_ l:list/length)
    (syntax/loc stx 'l.evidence)]
   [_ #f]))))

(define-syntax list-ref: (make-alias #'list-ref
  (lambda (stx) (syntax-parse stx
   [(_ l:list/length e)
    #:with i-stx (stx->num #'e)
    #:when (syntax-e #'i-stx)
    (let ([i (syntax-e #'i-stx)])
      (unless (< -1 i (syntax-e #'l.evidence))
        (bounds-error 'list-ref: #'l i))
      (syntax/loc stx (unsafe-list-ref l.expanded 'i-stx)))]
   [_ #f]))))

(define-syntax list-tail: (make-alias #'list-tail
  (lambda (stx) (syntax-parse stx
   [(_ l:list/length e)
    #:with i-stx (stx->num #'e)
    #:when (syntax-e #'i-stx)
    (let ([i (syntax-e #'i-stx)])
      (unless (< i (syntax-e #'l.evidence))
        (bounds-error 'list-tail: #'l i))
      (syntax/loc stx (unsafe-list-tail l.expanded 'i-stx)))]
   [_ #f]))))

(define-syntax append: (make-alias #'append
  (lambda (stx) (syntax-parse stx
   [(_ l1:list/length l2:list/length)
    #:with l+ (syntax-property (syntax/loc stx (append l1.expanded l2.expanded))
                lst-length-key (+ (syntax-e #'l1.evidence) (syntax-e #'l2.evidence)))
    (quasisyntax/loc stx l+)]
   [_ #f]))))

(define-syntax reverse: (make-alias #'reverse
  (lambda (stx) (syntax-parse stx
   [(_ l:list/length)
    #:with l+ (syntax-property (syntax/loc stx (reverse l.expanded))
                lst-length-key (syntax-e #'l.evidence))
    (quasisyntax/loc stx l+)]
   [_ #f]))))

(define-syntax map: (make-alias #'map
  (lambda (stx) (syntax-parse stx
   [(_ f l:list/length l*:list/length ...)
    #:with l+ (syntax-property
                (syntax/loc stx (map f l.expanded l*.expanded ...))
                lst-length-key
                (syntax-e #'l.evidence))
    (syntax/loc stx l+)]
   [_ #f]))))

