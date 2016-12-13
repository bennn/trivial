#lang reprovide

(only-in trivial/private/common
  [ttt-logger trivial-logger])

trivial/private/tailoring

(for-syntax
  (only-in trivial/private/common
    φ
    φ?
    φ-init
    φ-ref
    φ-set
    φ<=?

    [make-abstract-domain make-property-domain]
    [abstract-domain? property-domain?]
    in-domain?
    ⊥
    ⊤
    ⊥?
    ⊤?
    ⊓
    ⊓*
    [⊓ glb]
    [⊓* glb*]
    ⊔
    [⊔ lub]
    ⊔*
    [⊔* lub*]
    reduce
    reduce*))

(only-in trivial/private/format
  [F-dom format-string-domain])
(only-in trivial/private/function
  [A-dom arity-domain])
(only-in trivial/private/integer
  [I-dom integer-domain])
(only-in trivial/private/regexp
  [R-dom regexp-group-domain])
(for-syntax
  (only-in trivial/private/sequence-domain
    list-domain
    vector-domain
    vector-domain->list-domain
    list-domain->vector-domain
    [list-domain->I-dom list-domain->integer-domain]
    [I-dom->list-domain integer-domain->list-domain]
    [vector-domain->I-dom vector-domain->integer-domain]
    [I-dom->vector-domain integer-domain->vector-domain]
    list-domain-append*
    list-domain-car
    list-domain-cdr
    list-domain-cons
    list-domain-length
    list-domain-ref
    list-domain-reverse
    list-domain-set
    list-domain-slice
    vector-domain-append*
    vector-domain-length
    vector-domain-ref
    vector-domain-set
    vector-domain-slice))
