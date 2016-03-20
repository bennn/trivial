#lang typed/racket/base

;; Stronger types for regular expression matching.

;; Specification:
;; - Racket docs:
;;   http://docs.racket-lang.org/reference/regexp.html
;;
;; - Pregexp docs:
;;   http://ds26gte.github.io/pregexp/index.html
;;
;; - Racket source:
;;   https://github.com/racket/racket/blob/master/racket/src/racket/src/regexp.c

(provide
  regexp:
  pregexp:
  byte-regexp:
  byte-pregexp:
  define-regexp:
  let-regexp:

  regexp-match:

  (for-syntax
    rx-key
    rx-define
    rx-let)
)

(require
  (for-syntax
    (only-in racket/syntax format-id)
    typed/racket/base
    (only-in racket/format ~a)
    syntax/parse
    trivial/private/common))

;; =============================================================================

(begin-for-syntax
  (define errloc-key     'regexp-match:)

  (define (group-error str reason)
    (raise-argument-error
      errloc-key
      (format "Invalid regexp pattern (unmatched ~a)" reason)
      str))

  ;; Dispatch for counting groups
  (define (parse-groups v-stx)
    (define v (quoted-stx-value? v-stx))
    (cond
      [(string? v)        (parse-groups/string v #:src v-stx)]
      [(regexp? v)        (parse-groups/regexp v #:src v-stx)]
      [(pregexp? v)       (parse-groups/pregexp v #:src v-stx)]
      [(bytes? v)         (parse-groups/bytes v #:src v-stx)]
      [(byte-regexp? v)   (parse-groups/byte-regexp v #:src v-stx)]
      [(byte-pregexp? v)  (parse-groups/byte-pregexp v #:src v-stx)]
      [else               #f]))

  ;; Count the number of matched parentheses in a regexp pattern.
  ;; Raise an exception if there are unmatched parens.
  (define (parse-groups/untyped str #:src stx)
    (define last-index (- (string-length str) 1))
    (define ignore? (box #f))
    (let loop ([i 0] [in-paren '()] [num-groups 0])
      (if (> i last-index)
        (cond
         [(not (null? in-paren))
          (group-error str (format "'(' at index ~a" (car in-paren)))]
         [(unbox ignore?)
          (group-error str (format "'[' at index ~a" (car in-paren)))]
         [else
          num-groups])
        (if (unbox ignore?)
          (if (eq? #\] (string-ref str i))
            (begin (set-box! ignore? #f)
                   (loop (+ i 1) (cdr in-paren) num-groups))
            (loop (+ i 1) in-paren num-groups))
          (case (string-ref str i)
           [(#\[)
            ;; Ignore things between [ ... ]
            (set-box! ignore? #t)
            (loop (+ i 1) (cons i in-paren) num-groups)]
           [(#\()
            ;; Watch for (? patterns
            (if (and (< i last-index)
                     (eq? #\? (string-ref str (+ i 1))))
              (loop (+ i 2) (cons #f in-paren) num-groups)
              (loop (+ i 1) (cons i in-paren) num-groups))]
           [(#\))
            (cond
             [(and (< i last-index)
                   (or
                    (eq? #\? (string-ref str (+ i 1)))
                    (eq? #\* (string-ref str (+ i 1)))))
              ;; TODO starred group = may be #f
              #f]
             [(null? in-paren)
              (group-error str (format "')' at index ~a" i))]
             [(eq? #f (car in-paren))
              ;; Matched closing paren, but does not count as a group
              (loop (+ i 1) (cdr in-paren) num-groups)]
             [else
              (loop (+ i 1) (cdr in-paren) (+ 1 num-groups))])]
           [(#\\)
            (if (and (< i last-index)
                     (eq? #\\ (string-ref str (+ i 1))))
              (loop (+ i 3) in-paren num-groups)
              (loop (+ i 2) in-paren num-groups))]
           [(#\|)
            ;; Nope! Can't handle pipes
            #f]
           [else
            (loop (+ i 1) in-paren num-groups)])))))

  (define (parse-groups/string str #:src stx)
    (let ([ng (parse-groups/untyped str #:src stx)])
      (and ng (cons ng 'String))))

  (define (parse-groups/bytes b #:src stx)
    (let ([ng (parse-groups/untyped (~a b) #:src stx)])
      (and ng (cons ng 'Bytes))))

  (define (parse-groups/regexp rx #:src stx)
    (parse-groups/string (~a rx) #:src stx))

  (define parse-groups/pregexp
    parse-groups/regexp)

  (define (parse-groups/byte-regexp bx #:src stx)
    (parse-groups/bytes (~a bx) #:src stx))

  (define parse-groups/byte-pregexp
    parse-groups/byte-regexp)

  (define-values (rx-key rx? rx-define rx-let)
    (make-value-property 'rx:groups parse-groups))
  (define-syntax-class/predicate pattern/groups rx?)

)
;; -----------------------------------------------------------------------------
;; --- Other helpers

(begin-for-syntax
  (define (infer-return-type pattern-sym arg-stx)
    (if (and
          (or (eq? pattern-sym 'String)
              (eq? pattern-sym 'Regexp))
          (or (syntax-parse arg-stx
                ((x:str) #t)
                ((x) #:when (bytes? (syntax-e #'x)) #f)
                (_ #t))))
      'String
      'Bytes))
)

;; -----------------------------------------------------------------------------

(define-syntax (define-matcher* stx)
  (syntax-parse stx
   [(_ f*:id ...)
    #:with (f+* ...) (for/list ([f (in-list (syntax-e #'(f* ...)))])
                       (format-id stx "~a:" (syntax-e f)))
    #`(begin
        (define-syntax f+* (make-alias #'f*
          (lambda (stx) (syntax-parse stx
           [(_ pat:pattern/groups)
            (syntax-property
              (syntax/loc stx (f* pat.expanded))
              rx-key
              #'pat.evidence)]
           [_ #f])))) ...)]))

(define-matcher* regexp pregexp byte-regexp byte-pregexp)

(define-syntax define-regexp: (make-keyword-alias 'define rx-define))
(define-syntax let-regexp: (make-keyword-alias 'let rx-let))

(define-syntax regexp-match: (make-alias #'regexp-match
  (lambda (stx) (syntax-parse stx
   [(_ pat:pattern/groups arg* ...)
    #:with (num-groups . type-sym) (syntax/loc stx pat.evidence)
    ;; TODO keep source location in type-sym, stop using format-id
    ;;  (Is it really that bad?)
    #:with return-type (format-id stx "~a" (infer-return-type (syntax-e #'type-sym)
                                                              #'(arg* ...)))
    #:with (index* ...) (for/list ([i (in-range (syntax-e #'num-groups))]) i)
    (syntax/loc stx
      (let ([maybe-match (regexp-match pat.expanded arg* ...)])
        (if maybe-match
          (let ([m : (Listof (Option return-type)) maybe-match])
            (list (car maybe-match)
                  (begin (set! m (cdr m))
                         (or (car m) (error 'regexp-match: (format "Internal error at result index ~a, try using Racket's regexp-match" 'index*))))
                  ...))
          #f)))]
   [_ #f]))))

