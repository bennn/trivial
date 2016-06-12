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
    (only-in racket/unsafe/ops unsafe-string-ref)
    syntax/parse
    trivial/private/common))

;; =============================================================================

(begin-for-syntax
  (define errloc-key     'regexp-match:)

  (define (group-error stx str reason)
    (raise-user-error errloc-key
      "(~a:~a) Invalid regexp pattern (unmatched ~a) in ~a"
      (syntax-line stx)
      (syntax-column stx)
      reason
      str))

  ;; Dispatch for counting groups
  ;; On success, return (Pairof Type (Listof Boolean))
  ;; - type is probably the return type of matches
  ;; - booleans indicating "always succeeds" (#t) and "may fail" (#f)
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

  ;; (-> String #:src Syntax (Listof Boolean))
  (define (parse-groups/untyped str #:src stx)
    (define char->pos*
      (let ([H (unescaped-pos* str '(#\[ #\] #\( #\) #\| #\?))])
        (lambda (c)
          (hash-ref H c (lambda () (raise-user-error 'parse-groups "No position data for '~a' character" c))))))
    ;; -- check that [] are matched
    (define brack-ivl*
      (let* ([l-brack-pos* (char->pos* #\[)]
             [r-brack-pos* (char->pos* #\])]
             [r (pair-up l-brack-pos* r-brack-pos*)])
        ;; ?? okay for brackets to nest?
        (if (list? r)
          r
          (let ([brack-char (if (memv r l-brack-pos*) "[" "]")])
            (group-error stx str (format "'~a' at index ~a" brack-char r))))))
    ;; -- ignore characters between a pair of brackets
    (define-values (l-paren-pos* r-paren-pos* pipe-pos* ?-pos*)
      (apply values
        (for/list ([c (in-list '(#\( #\) #\| #\?))])
          (ivl-remove* brack-ivl* (char->pos* c)))))
    ;; -- check that () are matched
    (define paren-ivl*
      (let ([r (pair-up l-paren-pos* r-paren-pos*)])
        (if (list? r)
          r
          (let ([paren-char (if (memv r l-paren-pos*) "(" ")")])
            (group-error stx str (format "'~a' at index ~a" paren-char r))))))
    ;; -- groups = #parens.
    ;;    may fail to capture if has | outside (that are not nested in other parens)
    ;;    or ? after close
    (for/list ([ivl (in-list paren-ivl*)]
               #:when (not (has-?-before ivl ?-pos*)))
      (and
        (not (has-unguarded-pipe-before-or-after ivl paren-ivl* pipe-pos*))
        (not (has-?-after ivl ?-pos*)))))

  (define (has-?-before ivl ?-pos*)
    (define pos-before (+ 1 (car ivl))) ;; Well, just inside the paren.
    (for/or ([?pos (in-list ?-pos*)])
      (= pos-before ?pos)))

  (define (has-?-after ivl ?-pos*)
    (define pos-after (+ 1 (cdr ivl)))
    (for/or ([?pos (in-list ?-pos*)])
      (= pos-after ?pos)))

  (define (has-unguarded-pipe-before-or-after ivl paren-ivl* pipe-pos*)
    (define other-paren-ivl*
      (for/list ([ivl2 (in-list paren-ivl*)]
                 #:when (not (ivl< ivl ivl2)))
        ivl2))
    (define dangerous-pipe* (ivl-remove* other-paren-ivl* pipe-pos*))
    (not (null? dangerous-pipe*)))

  ;; Does not work for #\\ character
  (define (unescaped-pos* str c*)
    (define L (string-length str))
    (define escaped? (box #f))
    (define most-recent-char (box #f))
    (define (have-char-at-index? c i hist)
      (memv i (hash-ref hist c)))
    (define h-rev
      (for/fold ([hist (for/hasheq ([c (in-list c*)]) (values c '()))])
                ([i (in-range L)])
        (define char (unsafe-string-ref str i))
        (cond
         [(unbox escaped?)
          (when (or (not (eq? #\\ char))
                    (eq? #\[ (unbox most-recent-char)))
            (set-box! escaped? #f))
          hist]
         [(eq? #\\ char)
          (set-box! escaped? #t)
          hist]
         ;; --- special case for singleton <rng>,
         ;;     documented at `http://docs.racket-lang.org/reference/regexp.html`
         [(and (eq? #\] char)
               (or (have-char-at-index? #\[ (- i 1) hist)        ;; []] pattern
                   (and (have-char-at-index? #\[ (- i 2) hist)
                        (eq? #\^ (string-ref str (- i 1))))))    ;; [^]] pattern
          hist]
         [else
          (let ([i* (hash-ref hist char #f)])
            (if i*
              (begin
                (set-box! most-recent-char char)
                (hash-set hist char (cons i i*)))
              hist))])))
    ;; -- reverse all saved lists
    (for/hasheq ([(c i*) (in-hash h-rev)])
      (values c (reverse i*))))

  ;; (define-type Ivl (Pairof Natural Natural))

  ;; Match a list of left indices with a list of right indices.
  ;; Return a list of pairs on success
  ;;  and the unmatched index on failure.
  ;; (-> (Listof Natural) (Listof Natural) (U Natural (Listof Ivl)))
  (define (pair-up l* r*)
    (let loop ([i 0] [l* l*] [r* r*] [prev* '()])
      (cond
       [(null? r*)
        (if (null? l*)
          (if (null? prev*)
            '()             ;; good
            (car prev*))    ;; bad
          (car l*))]        ;; bad
       [(= i (car r*))
        (if (null? prev*)
          i
          (let ([r (loop (+ i 1) l* (cdr r*) (cdr prev*))])
            (if (integer? r)
              r
              (ivl-insert (cons (car prev*) i) r))))]
       [(or (null? l*) (< i (car l*)))
        (loop (+ i 1) l* r* prev*)]
       [(= i (car l*))
        (loop (+ i 1) (cdr l*) r* (cons i prev*))])))

  ;; Assume `ivl*` is sorted by left position
  ;; Insert `ivl` in sorted order
  ;; (-> Ivl (Listof Ivl) (Listof Ivl))
  (define (ivl-insert ivl ivl*)
    (cond
     [(null? ivl*)
      (list ivl)]
     [(< (car ivl) (caar ivl*))
      (cons ivl ivl*)]
     [else
      (cons (car ivl*) (ivl-insert ivl (cdr ivl*)))]))

  (define (ivl-remove* ivl* i*)
    (for/list ([i (in-list i*)]
               #:when (not (for/or ([ivl (in-list ivl*)]) (in-ivl? ivl i))))
      i))

  (define (ivl< ivl1 ivl2)
    (and (< (car ivl2) (car ivl1))
         (< (cdr ivl1) (cdr ivl2))))

  (define (in-ivl? ivl i)
    (and (< (car ivl) i)
         (< i (cdr ivl))))

  (define (infer-return-type pattern-sym arg-stx)
    (if (and
          (or (eq? pattern-sym 'String)
              (eq? pattern-sym 'Regexp))
          (or (syntax-parse arg-stx
                ((x:str) #t)
                ((x) #:when (bytes? (syntax-e #'x)) #f)
                ;; TODO ;; ((x) #:when (port? (syntax-e #'x)) #f)
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
    #:with (type-sym . capture?*)
           (syntax/loc stx pat.evidence)
    #:with return-type
           (format-id stx "~a" (infer-return-type (syntax-e #'type-sym) #'(arg* ...)))
    #:with (group-type* ...)
           (let ([stx-never-fail (syntax/loc stx return-type)]
                 [stx-may-fail (syntax/loc stx (U #f return-type))])
             (for/list ([c-stx (in-list (syntax-e #'capture?*))])
               (if (syntax-e c-stx)
                 stx-never-fail
                 stx-may-fail)))
    (syntax/loc stx
      (let ([maybe-match (regexp-match pat.expanded arg* ...)])
        (if maybe-match
          (cast maybe-match (List return-type group-type* ...))
          #f)))]
   [_ #f]))))
