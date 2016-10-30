#lang racket/base

;; Usage:
;;   raco trivial FILE.rkt
;; Will report all the optimizations that took place in it.

;; TODO
;; - automatically (require trivial)

(provide
  collect-and-summarize
)

(require
  (only-in racket/file delete-directory/files)
  (only-in racket/format ~a ~r)
  (only-in racket/list last)
  (only-in racket/logging with-logging-to-port)
  (only-in racket/string string-split string-prefix? string-contains?)
  (only-in racket/system process)
  (only-in trivial trivial-logger)
  racket/path
  syntax/modread
)

;; =============================================================================

(define *ANNIHILATE* (make-parameter #f))
(define TRIVIAL-LOG-PREFIX "ttt:")

(define-syntax-rule (debug msg arg* ...)
  (begin
    (display "[DEBUG] ")
    (printf msg arg* ...)
    (newline)))

(define (log->data ln)
  ;(printf "parsing ~a~n" ln)
  (string->symbol
    (cadr (regexp-match #rx"CHECK. '(.+?)'" ln))))

(define (rnd n)
  (~r n #:precision '(= 2)))

(define (summarize fname H M)
  (summarize-sexp fname H M))

(define (summarize-sexp fname H M)
  (printf "(~a" fname)
  (define-values (kv* pad-to) (hash->kv+pad H))
  (for ([kv (in-list (sort kv* > #:key cdr))])
    (define k (car kv))
    (define num-hits (cdr kv))
    (define num-miss (hash-ref M k 0))
    (define total (+ num-hits num-miss))
    (define pct (rnd (* 100 (/ num-hits total))))
    (newline)
    (printf "  (~a\t~a\t~a\t~a)" (~a k #:min-width pad-to) num-hits num-miss pct))
  (printf ")\n"))

(define (summarize-ascii H)
  (define msg "Summary of trivial CHECK+S:")
  (displayln msg)
  (displayln (make-string (string-length msg) #\=))
  (define-values (kv* pad-to) (hash->kv+pad H))
  (for ([kv (in-list (sort kv* > #:key cdr))])
    (displayln (string-append
      "- "
      (~a (car kv) #:min-width pad-to)
      "\t"
      (number->string (cdr kv))))))

(define (hash->kv+pad H)
  (for/fold ([acc '()]
             [pad-to 0])
            ([(k v) (in-hash H)])
    (values (cons (cons k v) acc) (max pad-to (string-length (symbol->string k))))))

(define (hit? line)
  (string-contains? line "CHECK+"))

(define (miss? line)
  (string-contains? line "CHECK-"))

(define (make-counter)
  (let* ([H (make-hasheq)]
         [H++ (lambda (k)
                (define old (hash-ref H k (lambda () #f)))
                (if old
                  (hash-set! H k (+ old 1))
                  (hash-set! H k 1)))])
    (values H H++)))

(define (compile-and-log fname)
  ;; TODO bug maybe here
  (with-logging-to-port (current-output-port)
    (lambda ()
      (parameterize ([current-namespace (make-base-namespace)])
        (with-module-reading-parameterization (lambda ()
          (expand (with-input-from-file fname read-syntax))
          (void)))))
    #:logger trivial-logger 'info))

(define (collect-and-summarize fname)
   (define-values (_in _out) (make-pipe))
   (unless (with-handlers ([exn:fail? (lambda (exn) #f)])
             (parameterize ([current-output-port _out])
               (compile-and-log fname)
               #t))
     (printf "RETRY ~a~n" fname)
     #;(with-output-to-file "retries.txt" #:exists 'append
       (lambda () (displayln fname))))
   (define-values (H H++) (make-counter))
   (define-values (M M++) (make-counter))
   (define num-lines (box 0))
   (define (subprocess-read _in)
     (for ([line (in-lines _in)])
       (set-box! num-lines (+ 1 (unbox num-lines)))
       (cond
        [(regexp-match? #rx"CHECK" line)
         (cond
          [(miss? line)
           (M++ (log->data line))]
          [else
           (H++ (log->data line))]
          #;[else
           (printf "WARNING: error parsing log message ~a\n" line)])]
        [else
         (void)])))
   (close-output-port _out)
   (subprocess-read _in)
   ;; -- close pipe ports
   (close-input-port _in)
   ;; --
   (summarize fname H M))

;; -----------------------------------------------------------------------------

(module+ main
  (require racket/cmdline racket/pretty)
  (command-line
   #:once-each
   [("--clean" "--all") "Make clean before running" (*ANNIHILATE* #t)]
   #:args (fname)
   (define v (collect-and-summarize fname))
   (pretty-print v)
   (void)))
