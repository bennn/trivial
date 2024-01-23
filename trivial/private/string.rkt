#lang racket/base

;; Strings, Byte Strings, and Characters (for now, the chars should move)

(provide
  (for-syntax C-dom S-dom B-dom)
  (rename-out
   [-string-length string-length]
   [-string-ref string-ref]
   #;[-string-set! string-set!]
   [-substring substring]
   [-string-append string-append]
   [-string->number string->number]
   [-bytes-length bytes-length]
   [-bytes-ref bytes-ref]
   #;[-bytes-set! bytes-set!]
   [-subbytes subbytes]
   [-bytes-append bytes-append]))

;; -----------------------------------------------------------------------------

(require
  syntax/parse
  trivial/private/tailoring
  (only-in trivial/private/integer
    I-dom)
  (prefix-in τ- (only-in typed/racket/base
    string-length
    string-ref
    string-set!
    substring
    string-append
    string->number assert complex?
    ;; ---
    bytes-length
    bytes-ref
    bytes-set!
    subbytes
    bytes-append))
  (prefix-in λ- (only-in racket/base
    string-length
    string-ref
    string-set!
    substring
    string-append
    string->number
    ;; ---
    bytes-length
    bytes-ref
    bytes-set!
    subbytes
    bytes-append))
  (for-syntax
    typed/untyped-utils
    syntax/parse
    racket/syntax
    racket/base
    trivial/private/common
    (only-in trivial/private/sequence-domain
      format-bounds-error
      format-slice-error)))

;; =============================================================================

(define-for-syntax C-dom
  (make-abstract-domain C
   [c:char
    (syntax-e #'c)]))

;; -----------------------------------------------------------------------------

(define-for-syntax S-dom
  ;; TODO use sets, so regexp patterns "[0-9]" can cover many
  (make-abstract-domain S
   [s:str
    (syntax-e #'s)]))

(define-tailoring (-string-length [e ~> e+ (φ [S-dom ↦ s])])
  #:with +string-length (τλ #'τ-string-length #'λ-string-length)
  #:= (⊥? S-dom s)
      (+string-length e+)
  #:+ #t
      '#,(string-length s)
  #:φ (φ-set (φ-init) I-dom (if (⊥? S-dom s) (⊥ I-dom) (string-length s))))

(define-tailoring (-string-ref [e0 ~> e0+ (φ0 [S-dom ↦ s])] [e1 ~> e1+ (φ1 [I-dom ↦ k])])
  #:with +string-ref (τλ #'τ-string-ref #'λ-string-ref)
  (define any-bot? (or (⊥? S-dom s) (⊥? I-dom k)))
  #:= any-bot?
      (+string-ref e0+ e1+)
  #:+ (and (<= 0 k) (< k (string-length s)))
      '#,(string-ref s k)
  #:- #true
      (format-bounds-error #'e0+ k)
  #:φ (φ-set (φ-init) C-dom (if any-bot? (⊥ C-dom) (string-ref s k))))

(define-tailoring (-string-set! [e0 ~> e0+ (φ0 [S-dom ↦ s])] [e1 ~> e1+ (φ1 [I-dom ↦ k])] [e2 ~> e2+ (φ2 [C-dom ↦ c])])
  #:with +string-set! (τλ #'τ-string-set! #'λ-string-set!)
  (define any-bot? (or (⊥? S-dom s) (⊥? I-dom k) (⊥? C-dom c)))
  (define safe? (and (<= 0 k) (< k (string-length s))))
  (define str+
    (if (or any-bot? (not safe?))
      (⊥ S-dom)
      (string-append (substring s 0 k) (string c) (substring s (+ k 1)))))
  #:= any-bot?
      (+string-set! e0+ e1+ e2+)
  #:+ safe?
      (+string-set! e0+ e1+ e2+)
  #:- #true
      (format-bounds-error #'e0+ k)
  #:φ (φ-set (φ-init) S-dom (if any-bot? (⊥ S-dom) str+)))

(define-tailoring (-substring1 [e0 ~> e0+ (φ0 [S-dom ↦ s])] [e1 ~> e1+ (φ1 [I-dom ↦ i])])
  #:with +substring (τλ #'τ-substring #'λ-substring)
  (define any-bot? (or (⊥? S-dom s) (⊥? I-dom i)))
  #:= any-bot?
      (+substring e0+ e1+)
  #:+ (and (<= 0 i) (< i (string-length s)))
      '#,(substring s i)
  #:- #true
      (format-bounds-error #'e0+ i)
  #:φ (φ-set (φ-init) S-dom (if any-bot? (⊥ S-dom) (substring s i))))

(define-tailoring (-substring2 [e0 ~> e0+ (φ0 [S-dom ↦ s])] [e1 ~> e1+ (φ1 [I-dom ↦ i1])] [e2 ~> e2+ (φ2 [I-dom ↦ i2])])
  #:with +substring (τλ #'τ-substring #'λ-substring)
  (define any-bot? (or (⊥? S-dom s) (⊥? I-dom i1) (⊥? I-dom i2)))
  #:= any-bot?
      (+substring e0+ e1+ e2+)
  #:+ (and (<= 0 i1) (<= i1 i2) (<= i2 (string-length s)))
      '#,(substring s i1 i2)
  #:- #true
      (cond
       [(or (< i1 0) (<= (string-length s) i1))
        (format-bounds-error #'e0+ i1)]
       [(or (< i2 0) (< (string-length s) i2))
        (format-bounds-error #'e0+ i2)]
       [else
        (format-slice-error #'e0+ i1 i2)])
  #:φ (φ-set (φ-init) S-dom (if any-bot? (⊥ S-dom) (substring s i1 i2))))

(define-syntax (-substring stx)
  (syntax-parse stx
   [(_ e0 e1)
    (syntax/loc stx (-substring1 e0 e1))]
   [(_ . e*)
    (syntax/loc stx (-substring2 . e*))]
   [_:id
    (syntax/loc stx -substring2)]))

(define-tailoring (-string-append [e* ~> e+* (φ [S-dom ↦ s*])] ...)
  #:with +string-append (τλ #'τ-string-append #'λ-string-append)
  #:= (for/or ([s (in-list s*)])
        (⊥? S-dom s))
      (+string-append e+* ...)
  #:+ #t
      '#,(apply string-append s*)
  #:φ (φ-set (φ-init) S-dom (reduce* S-dom string-append "" s*)))

(define-tailoring (-string->number [e ~> e+ (φ [S-dom ↦ s])])
  #:with +string->number (τλ #'τ-string->number #'λ-string->number)
  #:= (or (⊥? S-dom s)
          (not (syntax-local-typed-context?))
          (not (string->number s)))
      (+string->number e+)
  #:+ #t
      (τ-assert (+string->number e+) τ-complex?)
  #:φ (φ-init))

;; -----------------------------------------------------------------------------

(define-for-syntax B-dom
  (make-abstract-domain B
   [b
    (let ([v (syntax-e #'b)])
      (and (bytes? v) v))]))

(define-for-syntax (format-bytes-error stx)
  (format "[~a:~a] expected a byte in '~a'"
          (syntax-source stx)
          (syntax-line stx)
          (syntax->datum stx)))

(define-tailoring (-bytes-length [e ~> e+ (φ [B-dom ↦ b])])
  #:with +bytes-length (τλ #'τ-bytes-length #'λ-bytes-length)
  #:= (⊥? B-dom b)
      (+bytes-length e+)
  #:+ #t
      '#,(bytes-length b)
  #:φ (φ-set (φ-init) I-dom (if (⊥? B-dom b) (⊥ I-dom) (bytes-length b))))

(define-tailoring (-bytes-ref [e0 ~> e0+ (φ0 [B-dom ↦ b])] [e1 ~> e1+ (φ1 [I-dom ↦ k])])
  #:with +bytes-ref (τλ #'τ-bytes-ref #'λ-bytes-ref)
  (define any-bot? (or (⊥? B-dom b) (⊥? I-dom k)))
  #:= any-bot?
      (+bytes-ref e0+ e1+)
  #:+ (and (<= 0 k) (< k (bytes-length b)))
      '#,(bytes-ref b k)
  #:- #true
      (format-bounds-error #'e0+ k)
  #:φ (φ-set (φ-init) I-dom (if any-bot? (⊥ I-dom) (bytes-ref b k))))

(define-tailoring (-bytes-set! [e0 ~> e0+ (φ0 [B-dom ↦ b])] [e1 ~> e1+ (φ1 [I-dom ↦ k])] [e2 ~> e2+ (φ2 [I-dom ↦ i])])
  #:with +bytes-set! (τλ #'τ-bytes-set! #'λ-bytes-set!)
  (define any-bot? (or (⊥? B-dom b) (⊥? I-dom k) (⊥? I-dom i)))
  (define safe? (and (<= 0 k) (< k (bytes-length b)) (<= 0 i 256)))
  (define bytes+
    (if (or any-bot? (not safe?))
      (⊥ B-dom)
      (bytes-append (subbytes b 0 k) (bytes i) (subbytes b (+ k 1)))))
  #:= any-bot?
      (+bytes-set! e0+ e1+ e2+)
  #:+ safe?
      '#,bytes+
  #:- #true
      (cond
       [(or (< k 0) (<= (bytes-length b) k))
        (format-bounds-error #'e0+ k)]
       [else
        (format-bytes-error #'e2+ i)])
  #:φ (φ-set (φ-init) B-dom (if any-bot? (⊥ B-dom) bytes+)))

(define-tailoring (-subbytes1 [e0 ~> e0+ (φ0 [B-dom ↦ b])] [e1 ~> e1+ (φ1 [I-dom ↦ i])])
  #:with +subbytes (τλ #'τ-subbytes #'λ-subbytes)
  (define any-bot? (or (⊥? B-dom b) (⊥? I-dom i)))
  #:= any-bot?
      (+subbytes e0+ e1+)
  #:+ (and (<= 0 i) (< i (bytes-length b)))
      '#,(subbytes b i)
  #:- #true
      (format-bounds-error #'e0+ i)
  #:φ (φ-set (φ-init) B-dom (if any-bot? (⊥ B-dom) (subbytes b i))))

(define-tailoring (-subbytes2 [e0 ~> e0+ (φ0 [B-dom ↦ b])] [e1 ~> e1+ (φ1 [I-dom ↦ i1])] [e2 ~> e2+ (φ2 [I-dom ↦ i2])])
  #:with +subbytes (τλ #'τ-subbytes #'λ-subbytes)
  (define any-bot? (or (⊥? B-dom b) (⊥? I-dom i1) (⊥? I-dom i2)))
  #:= any-bot?
      (+subbytes e0+ e1+ e2+)
  #:+ (and (<= 0 i1) (<= i1 i2) (<= i2 (bytes-length b)))
      '#,(subbytes b i1 i2)
  #:- #true
      (cond
       [(or (< i1 0) (<= (bytes-length b) i1))
        (format-bounds-error #'e0+ i1)]
       [(or (< i2 0) (< (bytes-length b) i2))
        (format-bounds-error #'e0+ i2)]
       [else
        (format-slice-error #'e0+ i1 i2)])
  #:φ (φ-set (φ-init) B-dom (if any-bot? (⊥ B-dom) (subbytes b i1 i2))))

(define-syntax (-subbytes stx)
  (syntax-parse stx
   [(_ e0 e1)
    (syntax/loc stx (-subbytes1 e0 e1))]
   [(_ . e*)
    (syntax/loc stx (-subbytes2 . e*))]
   [_:id
    (syntax/loc stx -subbytes2)]))

(define-tailoring (-bytes-append [e* ~> e+* (φ [B-dom ↦ b*])] ...)
  #:with +bytes-append (τλ #'τ-bytes-append #'λ-bytes-append)
  #:= (for/or ([b (in-list b*)])
        (⊥? B-dom b))
      (+bytes-append e+* ...)
  #:+ #t
      '#,(apply bytes-append b*)
  #:φ (φ-set (φ-init) B-dom (reduce* B-dom bytes-append #"" b*)))

