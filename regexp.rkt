#lang typed/racket/base

;; Stronger types for regular expression matching.
;;
;; TODO use syntax-class to abstract over local-expands / check num-groups
;; TODO groups can be #f when using | ... any other way?

(provide
  regexp:       define-regexp:       let-regexp:
  pregexp:      define-pregexp:      let-pregexp:
  byte-regexp:  define-byte-regexp:  let-byte-regexp:
  byte-pregexp: define-byte-pregexp: let-byte-pregexp:
  ;; Expression and definition forms that try checking their argument patterns.
  ;; If check succeeds, will remember the number of pattern groups
  ;; for calls to `regexp-match:`.

  regexp-match:
  ;; (-> Pattern String Any * (U #f (List String *N+1)))
  ;; Match the regular expression pattern against a string.
  ;; If the pattern is determined statically, result will be either #f
  ;;  or a list of N+1 strings, where N is the number of groups specified
  ;;  the pattern.
  ;; Will raise a compile-time exception if the pattern contains unmatched groups.
)

(require (for-syntax
  typed/racket/base
  (only-in racket/format ~a)
  (only-in racket/syntax format-id)
  syntax/id-table
  syntax/parse
  trivial/private/common
))

;; =============================================================================

(define-for-syntax num-groups-key 'regexp-match:num-groups)
(define-for-syntax errloc-key     'regexp-match:)
(define-for-syntax id+num-groups (make-free-id-table))

;; (define-matcher f)
;; Expand to two forms:
;; - (f: arg)
;; - (define-f: id arg)
;; The first is for statically-checked patterns in expressions,
;;  the second is for patterns in definitions.
(define-syntax define-matcher
  (syntax-parser
   [(_ f:id)
    #:with f: (format-id #'f "~a:" (syntax-e #'f))
    #:with let-f: (format-id #'f "let-~a:" (syntax-e #'f))
    #:with define-f: (format-id #'f "define-~a:" (syntax-e #'f))
    #'(begin
        ;; For expressions, (regexp: val)
        (define-syntax f:
          (syntax-parser
           [g:id
            (syntax/loc #'g f)]
           [(_ pat-stx)
            #:with pat-stx+ (expand-expr #'pat-stx)
            #:with (num-groups . T) (count-groups #'pat-stx+)
            (syntax-property #'(f pat-stx+)
              num-groups-key
              (cons (syntax-e #'num-groups) #'T))]
           [(_ arg* (... ...))
            #'(f arg* (... ...))]))
        ;; For lets, (let-regexp: ([id val]) ...)
        (define-syntax let-f:
          (syntax-parser
           [(_ ([name:id pat-stx]) e* (... ...))
            #:with pat-stx+ (expand-expr #'pat-stx)
            #:with (num-groups . T) (count-groups #'pat-stx+)
            (free-id-table-set! id+num-groups
                                #'name
                                (cons (syntax-e #'num-groups) #'T))
            #'(let ([name pat-stx+]) e* (... ...))]
           [(_ arg* (... ...))
            #'(let arg* (... ...))]))
        ;; For definitions, (define-regexp: id val)
        (define-syntax define-f:
          (syntax-parser
           [(_ name:id pat-stx)
            #:with pat-stx+ (expand-expr #'pat-stx)
            #:with (num-groups . T) (count-groups #'pat-stx+)
            (free-id-table-set! id+num-groups
                                #'name
                                (cons (syntax-e #'num-groups) #'T))
            #'(define name pat-stx+)]
           [(_ arg* (... ...))
            #'(define arg* (... ...))]))) ]))

(define-matcher regexp)
(define-matcher pregexp)
(define-matcher byte-regexp)
(define-matcher byte-pregexp)

(define-syntax regexp-match:
  (syntax-parser
   [(f pat-stx arg* ...)
    #:with pat-stx+ (expand-expr #'pat-stx)
    #:with (num-groups . T) (count-groups #'pat-stx+)
    #:when (syntax-e #'num-groups)
    #:with (index* ...) #`#,(for/list ([i (in-range (syntax-e #'num-groups))]) i)
    #'(let ([maybe-match (regexp-match pat-stx+ arg* ...)])
        (if maybe-match
          (let ([m : (Listof (Option T)) maybe-match])
            (list (car maybe-match)
                  (begin (set! m (cdr m))
                         (or (car m) (error 'regexp-match: (format "Internal error at result index ~a, try using Racket's regexp-match" index*))))
                  ...))
          #f))]
   [f:id
    (syntax/loc #'f regexp-match)]
   [(f arg* ...)
    (syntax/loc #'f (regexp-match arg* ...))]))

;; -----------------------------------------------------------------------------

(define-for-syntax (group-error str reason)
  (raise-argument-error
    errloc-key
    (format "Valid regexp pattern (unmatched ~a)" reason)
    str))

;; Dispatch for counting groups
(define-for-syntax (count-groups v-stx)
  (cond
    [(syntax-property v-stx num-groups-key)
     => (lambda (x) x)]
    [(identifier? v-stx)
     (free-id-table-ref id+num-groups v-stx #f)]
    [(quoted-stx-value? v-stx)
     => (lambda (v)
     (cond
       [(string? v)        (count-groups/string v #:src v-stx)]
       [(regexp? v)        (count-groups/regexp v #:src v-stx)]
       [(pregexp? v)       (count-groups/pregexp v #:src v-stx)]
       [(bytes? v)         (count-groups/bytes v #:src v-stx)]
       [(byte-regexp? v)   (count-groups/byte-regexp v #:src v-stx)]
       [(byte-pregexp? v)  (count-groups/byte-pregexp v #:src v-stx)]
       [else               #f]))]
    [else  #f]))

;; Count the number of matched parentheses in a regexp pattern.
;; Raise an exception if there are unmatched parens.
(define-for-syntax (count-groups/untyped str #:src stx)
  (define last-index (- (string-length str) 1))
  (let loop ([i 0] [in-paren '()] [num-groups 0])
    (if (> i last-index)
      (if (null? in-paren)
        num-groups
        (group-error str (format "'(' at index ~a" (car in-paren))))
      (case (string-ref str i)
       [(#\()
        ;; Watch for (? patterns
        (if (and (< i last-index)
                 (eq? #\? (string-ref str (+ i 1))))
          (loop (+ i 2) (cons #f in-paren) num-groups)
          (loop (+ i 1) (cons i in-paren) num-groups))]
       [(#\))
        (cond
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
        (loop (+ i 1) in-paren num-groups)]))))

(define-for-syntax (count-groups/string str #:src stx)
  (cons (count-groups/untyped str #:src stx) (syntax/loc stx String)))

(define-for-syntax (count-groups/bytes b #:src stx)
  (cons (count-groups/untyped (~a b) #:src stx) (syntax/loc stx Bytes)))

(define-for-syntax (count-groups/regexp rx #:src stx)
  (count-groups/string (~a rx) #:src stx))

(define-for-syntax count-groups/pregexp
  count-groups/regexp)

(define-for-syntax (count-groups/byte-regexp bx #:src stx)
  (count-groups/bytes (~a bx) #:src stx))

(define-for-syntax count-groups/byte-pregexp
  count-groups/byte-regexp)
