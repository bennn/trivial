#lang typed/racket/base

(provide
  regexp-match!
  ;; (-> Pattern String Any * (U #f (List String *N+1)))
  ;; Match the regular expression pattern against a string.
  ;; If the pattern is determined statically, result will be either #f
  ;;  or a list of N+1 strings, where N is the number of groups specified
  ;;  the pattern.
  ;;
  ;; Will raise a compile-time exception if the pattern contains unmatched groups.
)

(require
  (for-syntax racket/base syntax/parse racket/syntax))

;; =============================================================================

(define-syntax regexp-match!
  (syntax-parser
   [(f pat-stx arg* ...)
    #:with num-groups (count-groups (format "~a" (syntax-e #'pat-stx)) #:src #'f)
    #:with ((index* . group-id*) ...)
           #`#,(for/list ([i (in-range (syntax-e #'num-groups))])
                 (cons i (format-id #'f "group-~a" i)))
    ;; Chaining list-ref?
    #'(let ([m (regexp-match pat-stx arg* ...)])
        (if m
          (let ([group-id* (or (list-ref m index*) (error 'regexp-match! "Internal error, try Racket's `regexp-match`"))] ...)
            (list (car m) group-id* ...))
          m))]
   [(f arg* ...)
    (syntax/loc #'f (regexp-match arg* ...))]))

(define-for-syntax (count-groups v #:src stx)
  (cond
   [(string? v)  (count-groups/string v #:src stx)]
   ;[(regexp? v)  (count-groups/regexp v #:src stx)]
   ;[(pregexp? v) (count-groups/pregexp v #:src stx)]
   [else         (error 'regexp-match! "Internal error on input" v)]))

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
  (error 'regexp-match! "Not implemented"))

(define-for-syntax (count-groups/regexp pxp #:src stx)
  (error 'regexp-match! "Not implemented"))

(define-for-syntax (group-error str reason)
  (raise-argument-error
    'regexp-match!
    (format "Valid regexp pattern (contains unmatched ~a)" reason)
    str))
