#lang reprovide

trivial/private/tailoring

(only-in trivial/private/common
  φ
  φ-init
  φ-ref
  φ-set
  φ<=?

  make-abstract-domain
  ⊥
  ⊤
  ⊤-msg
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
  reduce*)
