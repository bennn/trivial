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
    (only-in racket/list range)
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

  ;; Handle pipes
  ;; If there is a pipe, everything is nullable, but we know the number of groups
  (define (parse-groups/untyped str #:src stx)
    (define alt* (string->alt* str))
    (cond
     [(null? alt*)
      (list 0 '())]
     [(null? (cdr alt*))
      (parse-groups-for-alt (car alt*) #:src stx)]
     [else
      (define num-groups
        (for/fold ([num-groups 0])
                  ([alt (in-list alt*)])
          (define ng+null* (parse-groups-for-alt alt #:src stx))
          (+ num-groups (car ng+null*))))
      (list num-groups (range num-groups))]))

  ;; Count the number of matched parentheses in a regexp pattern.
  ;; Raise an exception if there are unmatched parens.
  (define (parse-groups-for-alt str #:src stx)
    (define last-index (- (string-length str) 1))
    (define in-square? (box #f))
    (let loop ([i 0] [in-paren '()] [num-groups 0] [null-idx* '()])
      (if (> i last-index)
        (cond
         [(not (null? in-paren))
          (group-error str (format "'(' at index ~a" (car in-paren)))]
         [(unbox in-square?)
          (group-error str (format "'[' at index ~a" (car in-paren)))]
         [else
          (list num-groups null-idx*)])
        (if (unbox in-square?)
          (if (eq? #\] (string-ref str i))
            (begin (set-box! in-square? #f)
                   (loop (+ i 1) (cdr in-paren) num-groups null-idx*))
            (loop (+ i 1) in-paren num-groups null-idx*))
          (case (string-ref str i)
           [(#\[)
            ;; Ignore things between [ ... ]
            (set-box! in-square? #t)
            (loop (+ i 1) (cons i in-paren) num-groups null-idx*)]
           [(#\()
            ;; Watch for (? patterns
            (if (and (< i last-index)
                     (eq? #\? (string-ref str (+ i 1))))
              (loop (+ i 2) (cons #f in-paren) num-groups null-idx*)
              (loop (+ i 1) (cons i in-paren) num-groups null-idx*))]
           [(#\))
            (cond
             [(null? in-paren)
              (group-error str (format "')' at index ~a" i))]
             [(eq? #f (car in-paren))
              ;; Matched closing paren, but does not count as a group
              (loop (+ i 1) (cdr in-paren) num-groups null-idx*)]
             [(and (< i last-index)
                   (or
                    (eq? #\? (string-ref str (+ i 1)))
                    (eq? #\* (string-ref str (+ i 1)))))
              ;; group = may be #f
              (loop (+ i 1) (cdr in-paren) (+ 1 num-groups) (cons num-groups null-idx*))]
             [else
              (loop (+ i 1) (cdr in-paren) (+ 1 num-groups) null-idx*)])]
           [(#\\)
            (if (and (< i last-index)
                     (eq? #\\ (string-ref str (+ i 1))))
              (loop (+ i 3) in-paren num-groups null-idx*)
              (loop (+ i 2) in-paren num-groups null-idx*))]
           [(#\|)
            ;; Nope! Can't handle pipes
            (error 'internal-error "Found '|' character in regexp string.")]
           [else
            (loop (+ i 1) in-paren num-groups null-idx*)])))))

  (define (parse-groups/string str #:src stx)
    (let ([ng (parse-groups/untyped str #:src stx)])
      (and ng (cons 'String ng))))

  (define (parse-groups/bytes b #:src stx)
    (let ([ng (parse-groups/untyped (~a b) #:src stx)])
      (and ng (cons 'Bytes ng))))

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

  ;; Divide string into |-separated substrings (regex alternates)
  ;; Be wary of escaped | characters.
  (define (string->alt* str)
    (define L (string-length str))
    (let loop ([prev-i 0] [i 0])
      (cond
       [(= i L)
        ;; End of string, return last alternate
        (list (substring str prev-i i))]
       [(and (eq? (string-ref str i) #\|)
               (< 1 i)
               (not (and (eq? (string-ref str (- i 1)) #\\)
                         (eq? (string-ref str (- i 2)) #\\))))
        ;; Found a pipe, save current alternate
        (cons (substring str prev-i i)
              (loop (+ i 1) (+ i 1)))]
       [else
        ;; Nothing interesting, continue building alternate
        (loop prev-i (+ i 1))])))

  (define (intlist-union i* j*)
    (cond
     [(null? i*)
      j*]
     [(null? j*)
      i*]
     [(< (car i*) (car j*))
      (cons (car i*) (intlist-union (cdr i*) j*))]
     [(> (car i*) (car j*))
      (cons (car j*) (intlist-union i* (cdr j*)))]
     [else
      (cons (car i*) (intlist-union (cdr i*) (cdr j*)))]))

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
    #:with (type-sym num-groups null-idx*) (syntax/loc stx pat.evidence)
    ;; TODO keep source location in type-sym, stop using format-id
    ;;  (Is it really that bad?)
    #:with return-type (format-id stx "~a" (infer-return-type (syntax-e #'type-sym)
                                                              #'(arg* ...)))
    #:with (group-type* ...) (let ([null* (syntax->datum #'null-idx*)])
                               (for/list ([i (in-range (syntax-e #'num-groups))])
                                 (if (memv i null*)
                                   (syntax/loc stx (U #f return-type))
                                   (syntax/loc stx return-type))))
    (syntax/loc stx
      (let ([maybe-match (regexp-match pat.expanded arg* ...)])
        (if maybe-match
          (cast maybe-match (List return-type group-type* ...))
          #f)))]
   [_ #f]))))
