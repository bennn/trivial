#lang racket/base

(provide
  define-tailoring
  ;tailored-out
  (for-syntax
    typed/untyped-id
    (rename-out
      [typed/untyped-id τλ]))
)

(require
  (for-syntax
    racket/pretty
    racket/base
    racket/syntax
    syntax/parse
    typed/untyped-utils
    ;racket/provide-transform
    trivial/private/common))

;; =============================================================================

(begin-for-syntax
  (define-syntax-rule (typed/untyped-id t u)
    (if (syntax-local-typed-context?) t u))

  (define-syntax-class elab->
    (pattern (~datum ~>))
    (pattern (~datum ⇝)))

  (define-syntax-class map->
    (pattern (~datum ->))
    (pattern (~datum ↦)))

  (define-splicing-syntax-class elaboration
    (pattern (e0:id _:elab-> e0+:id (φ0:id [k*:id (~optional _:map->) v*:id] ...))
      #:with (e ...) #'((~var e0 ~>))
      #:with (e+ ...) #`(#:with e0+ #'#,(format-id #'e0 "~a.~~>" (syntax-e #'e0)))
      #:with (φ ...) #'((define φ0 (φ #'e0+))
                        (define v*
                          (let ([v (φ-ref φ0 k*)])
                            (if (⊤? k* v) (raise-user-error '~> (⊤-msg v)) v)))
                        ...)
    )
    (pattern (~seq (e*:id _:elab-> e+*:id (φ*:id [k**:id (~optional _:map->) v**:id] ...)) (~datum ...))
     #:with e... (format-id #'e* "...")
     #:with (e ...) #'((~var e* ~>) e...)
     #:with (e+ ...) #`(#:with (e+* e...)
                               #'(#,(format-id #'e* "~a.~~>" (syntax-e #'e*)) e...))
     #:with (φ ...) #'((define φ*
                         (map φ (syntax-e #'(e+* e...))))
                       (define-values (v** ...)
                         (values (for/list ([φi (in-list φ*)])
                                   (let ([v (φ-ref φi k**)])
                                     (if (⊤? k** v) (raise-user-error '~> (⊤-msg v)) v)))
                                 ...))))
  )

  (define-splicing-syntax-class guard
    (pattern (~seq #:with e1 e2)
     #:with (g ...) #'(#:with e1 e2))
    (pattern (~seq #:when e)
     #:with (g ...) #'(#:when e))
    (pattern (~seq #:fail-unless e1 e2)
     #:with (g ...) #'(#:fail-unless e1 e2)))

  (define-syntax-class definition
    (pattern ((~literal define) . e*)
     #:with d #'(define . e*)))

)

(define-syntax (define-tailoring stx)
  (syntax-parse stx
   [(_ (tailored-id:id pat*:elaboration ...)
       grd*:guard ...
       dfn*:definition ...
       (~optional (~seq #:= g= e=)
                  #:defaults ([g= #'#f] [e= #'#f]))
       (~optional (~seq #:+ g+ e+)
                  #:defaults ([g+ #'#f] [e+ #'#f]))
       (~optional (~seq #:- g- e-)
                  #:defaults ([g- #'#t] [e- #'(error 'tailored-id "cond failure")]))
       #:φ prop-expr)
    #:with error-id (string->symbol (substring (symbol->string (syntax-e #'tailored-id)) 1))
    #:with τ-tailored-id (format-id stx "τ~a" (syntax-e #'tailored-id))
    #:with λ-tailored-id (format-id stx "λ~a" (syntax-e #'tailored-id))
    #:with τλ-tailored-id (format-id stx "τλ~a" (syntax-e #'tailored-id))
    (syntax/loc stx
      (define-syntax (tailored-id new-stx)
        (syntax-parse new-stx
         [(_ pat*.e ... ...)
          grd*.g ... ...
          pat*.e+ ... ...
          pat*.φ ... ...
          dfn*.d ...
          (⊢ (cond
              [g= (log-ttt-check- 'error-id new-stx) (quasisyntax/loc new-stx e=)]
              [g+ (log-ttt-check+ 'error-id new-stx) (quasisyntax/loc new-stx e+)]
              [g- (raise-user-error 'error-id e-)])
             prop-expr)]
         [(_ . e*)
          #:with τλ-tailored-id (if (syntax-local-typed-context?)
                                  (syntax/loc new-stx τ-tailored-id)
                                  (syntax/loc new-stx λ-tailored-id))
          (syntax/loc new-stx
            (τλ-tailored-id . e*))]
         [_:id
          #:with τλ-tailored-id (if (syntax-local-typed-context?)
                                  (syntax/loc new-stx τ-tailored-id)
                                  (syntax/loc new-stx λ-tailored-id))
          (syntax/loc new-stx
            τλ-tailored-id)]))) ]
   [(_ tailored-id:id
       grd*:guard ...
       dfn*:definition ...
       (~optional (~seq #:= g= e=)
                  #:defaults ([g= #'#f] [e= #'#f]))
       (~optional (~seq #:+ g+ e+)
                  #:defaults ([g+ #'#f] [e+ #'#f]))
       (~optional (~seq #:- g- e-)
                  #:defaults ([g- #'#t] [e- #'(error 'tailored-id "cond failure")]))
       #:φ prop-expr)
    #:with error-id (string->symbol (substring (symbol->string (syntax-e #'tailored-id)) 1))
    (syntax/loc stx
      (define-syntax (tailored-id new-stx)
        (syntax-parse new-stx
         [_:id
          grd*.g ... ...
          dfn*.d ...
          (⊢ (cond
              [g= (log-ttt-check- 'error-id new-stx) (quasisyntax/loc new-stx e=)]
              [g+ (log-ttt-check+ 'error-id new-stx) (quasisyntax/loc new-stx e+)]
              [g- (raise-user-error 'error-id e-)])
             prop-expr)])))]))

;(define-syntax tailored-out
;  (make-provide-pre-transformer
;    (lambda (stx modes)
;      (syntax-parse stx
;       [(_ nm:id clause* ...)
;        #:with tmp (gensym (syntax-e #'nm))
;        ;; Move type declarations to the toplevel
;        (printf "deifning ~a\n" (syntax-e #'tmp))
;        (syntax-local-lift-module-end-declaration
;          (syntax/loc stx
;            (define (tmp stx)
;              (syntax-parse stx
;               clause* ... ))))
;        ;; Collect a flat list of provide specs & expand
;        (expand-export #'(rename-out [tmp nm]) modes)]))))
