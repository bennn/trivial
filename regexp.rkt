#lang typed/racket/base

;; TODO byte-regexp

(provide
  regexp:
  define-regexp:
  pregexp:

  regexp-match:
  ;; (-> Pattern String Any * (U #f (List String *N+1)))
  ;; Match the regular expression pattern against a string.
  ;; If the pattern is determined statically, result will be either #f
  ;;  or a list of N+1 strings, where N is the number of groups specified
  ;;  the pattern.
  ;;
  ;; Will raise a compile-time exception if the pattern contains unmatched groups.
)

(require (for-syntax
  racket/base
  racket/syntax
  syntax/id-table
  syntax/parse
))

;; =============================================================================

(define-for-syntax num-groups-key 'regexp-match:num-groups)
(define-for-syntax errloc-key     'regexp-match:)

(define-for-syntax id+num-groups (make-free-id-table))

;; (define-matcher f)
;; TODO document
(define-syntax define-matcher
  (syntax-parser
   [(_ f:id)
    #:with f: (format-id #'f "~a:" (syntax-e #'f))
    #:with define-f: (format-id #'f "define-~a:" (syntax-e #'f))
    #'(begin
        ;; For expressions, (regexp: val)
        (define-syntax f:
          (syntax-parser
           [(_ x:str)
            (syntax-property #'(f x)
              num-groups-key
              (count-groups #'x))]
           [(_ arg* (... ...))
            #'(f arg* (... ...))]))
        ;; For definitions, (define-regexp: id val)
        (define-syntax define-f:
          (syntax-parser
           [(_ name:id x)
            (free-id-table-set! id+num-groups #'name (count-groups #'x))
            #'(define name x)]
           [(_ arg* (... ...))
            #'(define arg* (... ...))]))) ]))

(define-matcher regexp)
(define-matcher pregexp)
(define-matcher byte-regexp)
(define-matcher byte-pregexp)

(define-syntax regexp-match:
  (syntax-parser
   [(f pat-stx arg* ...)
    #:with num-groups (count-groups #'pat-stx)
    #:when (syntax-e #'num-groups)
    #:with (index* ...) #`#,(for/list ([i (in-range (syntax-e #'num-groups))]) i)
    #'(let ([maybe-match (regexp-match pat-stx arg* ...)])
        (if maybe-match
          (let ([m : (Listof (Option String)) maybe-match])
            (list (car maybe-match)
                  (begin (set! m (cdr m))
                         (or (car m) (error 'regexp-match: (format "Internal error at result index ~a, try using Racket's regexp-match" index*))))
                  ...))
          #f))]
   [(f arg* ...)
    (syntax/loc #'f (regexp-match arg* ...))]))

;; -----------------------------------------------------------------------------

(define-for-syntax (count-groups v-stx)
  (let ([v (syntax-e v-stx)])
    (cond
     [(identifier? v-stx) (free-id-table-ref id+num-groups v-stx #f)]
     [(string? v)       (count-groups/string v #:src v-stx)]
     [(regexp? v)       (count-groups/regexp v #:src v-stx)]
     [(pregexp? v)      (count-groups/pregexp v #:src v-stx)]
     [(byte-regexp? v)  (count-groups/byte-regexp v #:src v-stx)]
     [(byte-pregexp? v) (count-groups/byte-pregexp v #:src v-stx)]
     [else  (error errloc-key (format "Internal error on input '~e'" v))])))

;; Count the number of matched parentheses in a regexp pattern.
;; Raise an exception if there are unmatched parens.
(define-for-syntax (count-groups/string str #:src stx)
  (define last-index (- (string-length str) 1))
  (let loop ([i 0] [in-paren #f] [num-groups 0])
    (if (> i last-index)
      (if in-paren
        (group-error str (format "'(' at index ~a" in-paren))
        num-groups)
      (case (string-ref str i)
       [(#\()
        (loop (+ i 1) i num-groups)]
       [(#\))
        (unless in-paren
          (group-error str (format "')' at index ~a" i)))
        (loop (+ i 1) #f (+ 1 num-groups))]
       [(#\\)
        (if (and (< i last-index) (eq? #\\ (string-ref str (+ i 1))))
          (loop (+ i 3) in-paren num-groups)
          (loop (+ i 2) in-paren num-groups))]
       [else
        (loop (+ i 1) in-paren num-groups)]))))

(define-for-syntax (count-groups/regexp rxp #:src stx)
  (error errloc-key "Not implemented"))

(define-for-syntax (count-groups/pregexp pxp #:src stx)
  (error errloc-key "Not implemented"))

(define-for-syntax (count-groups/byte-regexp rxp #:src stx)
  (error errloc-key "Not implemented"))

(define-for-syntax (count-groups/byte-pregexp pxp #:src stx)
  (error errloc-key "Not implemented"))

(define-for-syntax (group-error str reason)
  (raise-argument-error
    errloc-key
    (format "Valid regexp pattern (contains unmatched ~a)" reason)
    str))
