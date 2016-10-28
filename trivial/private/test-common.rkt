#lang racket/base

(provide
  test-compile-error
  check-trivial-log-sequence
)

(require
  rackunit
  racket/logging
  (only-in racket/list
    add-between)
  (only-in racket/string
    string-contains?
    string-join)
  (only-in trivial/private/common
    ttt-logger)
  (for-syntax
    racket/base
    syntax/parse))

;; =============================================================================

(define-syntax (test-compile-error stx)
  (syntax-parse stx
   [(_ #:require r-s* ...
       #:exn exn-rx
       e* ...)
    (syntax/loc stx
      (begin
        (check-exn exn-rx
          (lambda ()
            (compile-syntax #'(module t typed/racket/base (require r-s* ...) e*))))
        ...))]))

(define-syntax (check-trivial-log-sequence stx)
  (syntax-parse stx
   [(_ ?e ?type+sym*)
    (quasisyntax/loc stx
      (with-check-info* (list (make-check-location '#,(syntax->location stx)))
        (λ ()
          (define type+sym* ?type+sym*)
          (define inbox (intercept-ttt-log #'?e))
          (define log* (reverse (hash-ref inbox 'info)))
          (define len (length type+sym*))
          (define num-log* (length log*))
          (define fail-msg
            (format "too ~a log messages:~n  ~s"
              (if (< num-log* len) "few" "many")
              (string-join (add-between log* " ~n  "))))
          (check-equal? num-log* len fail-msg)
          ;; (printf fail-msg) (newline) ;; --- for debugging
          (when (= num-log* len)
            (for ([log (in-list log*)]
                  [ts (in-list type+sym*)])
              (define pat (format "~a '~a'" (car ts) (cadr ts)))
              (check-true (string-contains? log pat)
                (format "pattern '~a' not in log message: ~a" pat log))))
           (void))))]))

(define (intercept-ttt-log stx)
  (define inbox (make-hasheq '((debug . ()) (info . ()) (warning . ()) (error . ()) (fatal . ()))))
  (with-intercepted-logging
    (λ (l)
      (define lvl (vector-ref l 0))
      (define msg (vector-ref l 1))
      (when (eq? 'ttt (vector-ref l 3)) ;; filters out logs from racket/contract, etc
        (hash-set! inbox lvl (cons msg (hash-ref inbox lvl))))
      (void))
    (λ ()
      (compile-syntax stx))
    #:logger ttt-logger
    'info)
  inbox)

(define-for-syntax (syntax->location stx)
  (list (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)))
