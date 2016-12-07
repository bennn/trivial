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
  (only-in racket/logging with-intercepted-logging)
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
  (define-values (kv* pad-to) (hash->kv+pad H))
  (cons fname
    (for/list ([kv (in-list (sort kv* > #:key cdr))])
      (define k (car kv))
      (define num-hits (cdr kv))
      (define num-miss (hash-ref M k 0))
      (define total (+ num-hits num-miss))
      (define pct (rnd (* 100 (/ num-hits total))))
      (list (~a k #:min-width pad-to) num-hits num-miss pct))))

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

(define ((compile-file path+fname))
  (define-values (path fname)
    (let ([po (path-only path+fname)])
      (if po
        (values po (file-name-from-path path+fname))
        (values (current-directory) (if (path? path+fname)
                                      path+fname
                                      (string->path path+fname))))))
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-directory path])
    (with-module-reading-parameterization
      (λ ()
        (expand
          (with-input-from-file fname
            ;; TODO 2016-10-30 : gives bad error messages because no srcloc
            (λ () (read-syntax fname (current-input-port)))))
        (void)))))

;; -----------------------------------------------------------------------------
;; 2016-12-07 : copied from racket/logging v6.7.0.1
(define (receiver-thread receiver stop-chan intercept)
  (thread
   (lambda ()
     (define (clear-events)
       (let ([l (sync/timeout 0 receiver)])
         (when l ; still something to read
           (intercept l) ; interceptor gets the whole vector
           (clear-events))))
     (let loop ()
       (let ([l (sync receiver stop-chan)])
         (cond [(eq? l 'stop)
                ;; we received all the events we were supposed
                ;; to get, read them all (w/o waiting), then
                ;; stop
                (clear-events)]
               [else ; keep going
                (intercept l)
                (loop)]))))))

(define (with-intercepted-logging interceptor proc #:logger [logger #f]
                                  . log-spec)
  (let* ([orig-logger (current-logger)]
         ;; Unless we're provided with an explicit logger to monitor we
         ;; use a local logger to avoid getting messages that didn't
         ;; originate from proc. Since it's a child of the original logger,
         ;; the rest of the program still sees the log entries.
         [logger      (or logger (make-logger #f orig-logger))]
         [receiver    (apply make-log-receiver logger log-spec)]
         [stop-chan   (make-channel)]
         [t           (receiver-thread receiver stop-chan interceptor)])
    (begin0
        (parameterize ([current-logger logger])
          (proc))
      (channel-put stop-chan 'stop) ; stop the receiver thread
      (thread-wait t))))

;; end copy
;; -----------------------------------------------------------------------------

(define (collect-and-summarize fname)
   (define-values (H H++) (make-counter))
   (define-values (M M++) (make-counter))
   (define num-lines (box 0))
   (with-intercepted-logging
     (λ (le)
       (when (eq? 'info (vector-ref le 0))
         (define line (vector-ref le 1))
         (set-box! num-lines (+ 1 (unbox num-lines)))
         (cond
          [(regexp-match? #rx"CHECK" line)
           (cond
            [(miss? line)
             (M++ (log->data line))]
            [else
             (H++ (log->data line))]
            #;[else
             (printf "WARNING: failed to parse log message ~a\n" line)])]
          [else
           (void)])))
     (compile-file fname)
     #:logger trivial-logger 'info)
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
   (pretty-display v)
   (void)))
