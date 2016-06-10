#lang racket/base

;; Usage:
;;   raco trivial FILE.rkt
;; If file has the *TRIVIAL-LOG* parameter set at phase 1,
;;  this file will report all the optimizations that took place in it.

;; TODO
;; - automatically set LOG parameter
;; - automatically (require trivial)
;; - work for typed OR untyped files

(provide
  collect-and-summarize
)

(require
  (only-in racket/file delete-directory/files)
  (only-in racket/format ~a ~r)
  (only-in racket/list last)
  (only-in racket/string string-split string-prefix? string-contains?)
  (only-in racket/system process)
  racket/path
  syntax/modread
)

;; =============================================================================

(define TRIVIAL-LOG-PREFIX "[LOG")

(define *ANNIHILATE* (make-parameter #f))

(define-syntax-rule (debug msg arg* ...)
  (begin
    (display "[DEBUG] ")
    (printf msg arg* ...)
    (newline)))

(define (log->data ln)
  (string->symbol (last (string-split ln))))

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
  (define msg "Summary of trivial HITS:")
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

(define (remove-compiled ps)
  (define c-dir (build-path (or (path-only ps) (current-directory)) "compiled"))
  (define fname (path-replace-extension (file-name-from-path ps) "_rkt.zo"))
  (define c-file (build-path c-dir fname))
  (cond
   [(*ANNIHILATE*)
    (delete-directory/files c-dir #:must-exist? #f)]
   [(and (directory-exists? c-dir)
         (file-exists? c-file))
    (delete-file c-file)]
   [else
    (void)]))

(define (hit? line)
  (string-contains? line "HIT"))

(define (miss? line)
  (string-contains? line "MISS"))

(define (make-counter)
  (let* ([H (make-hasheq)]
         [H++ (lambda (k)
                (define old (hash-ref H k (lambda () #f)))
                (if old
                  (hash-set! H k (+ old 1))
                  (hash-set! H k 1)))])
    (values H H++)))

(define (collect-and-summarize fname)
   (remove-compiled fname)
   (define cmd (format "raco make ~a" fname))
   (define-values (in out pid err check-status) (apply values (process cmd)))
   (define-values (H H++) (make-counter))
   (define-values (M M++) (make-counter))
   (define num-lines (box 0))
   (define (subprocess-read)
     (for ([line (in-lines in)])
       (set-box! num-lines (+ 1 (unbox num-lines)))
       (cond
        [(string-prefix? line TRIVIAL-LOG-PREFIX)
         (cond
          [(miss? line)
           (M++ (log->data line))]
          [else
           (H++ (log->data line))]
          #;[else
           (printf "WARNING: error parsing log message ~a\n" line)])]
        [else
         (void)])))
   (let loop ()
     (case (check-status 'status)
      [(running)
       #;(debug "Subprocess running, reading output so far")
       (subprocess-read)
       (loop)]
      [(done-ok)
       (subprocess-read)
       #;(debug "Subprocess finished cleanly. Produced ~a lines of output." (unbox num-lines))]
      [(done-error)
       (parameterize ([current-output-port (current-error-port)])
         (for ([line (in-lines err)]) (displayln line)))
       (raise-user-error 'trace "Subprocess '~a' exited with an error" cmd)]))
   ;; -- close pipe ports
   (close-input-port in)
   (close-output-port out)
   (close-input-port err)
   ;; --
   (summarize fname H M))

;; -----------------------------------------------------------------------------

(module+ main
  (require racket/cmdline)
  (command-line
   #:once-each
   [("--clean" "--all") "Make clean before running" (*ANNIHILATE* #t)]
   #:args (fname)
   (collect-and-summarize fname)))

;; -----------------------------------------------------------------------------
;; -- trash

;(require
;  (for-syntax racket/base (only-in trivial/parameters *TRIVIAL-LOG*)))
;(begin-for-syntax (*TRIVIAL-LOG* #t))
;
;(define-namespace-anchor nsa)
;(define ns (namespace-anchor->namespace nsa))


   ;(with-module-reading-parameterization
   ;  (lambda ()
   ;    (call-with-input-file fname
   ;      (lambda (port)
   ;        (parameterize ([current-namespace ns])
   ;          (void (compile (read-syntax fname port))))))))
