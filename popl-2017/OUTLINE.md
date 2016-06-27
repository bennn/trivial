Outline of paper
---

- λ ∈ ℕ Σ
- expand is NOT guaranteed to make well-typed terms; you can call it on any expr.
- idea; values carry latent "type" information
  (but ya know, this is kind of just demonstrating that expand is useful)
- (afterwards) add set!
  nevermind how \tau handles it ... here are rules for expand to handle it
- note: OCaml is firm about maintaining a fast compiler, NOT a big standard library
-       thats a totally reasonable separation of concerns/labor

TODO
- use more RACKET, less math (@racket[...] for identifiers)


0 intro.scrbl
---
types, abstraction,
syntax extensions,
true refinement,
other promises


1 background.scrbl
---
define types
define macros (without the word 'macro')


2. examples.scrbl
---
carefully explain 2 extensions
state guarantees
prove properties
evaluations


3. discussion.scrbl
---
limits of the extensions (need a value),
suggestions to overcome limits (assertions, better analysis),
even more extensions + implications

"""
Our technique is implemented as a library of local transformations that
 compose to form a function @exact{$\elaborate$} defined over syntactically
 well-formed terms.
Using the library is a one-line change for existing programs; however, the
 user may wish to remove type casts made redundant by the elaboration.
"""


4. friends.scrbl
---
how to implement in
- typed clojure
- scala (macros or LMS)
- javascript
rust will be difficult


5. ending
---

0. finish sql search
1. revise 3,4 with MODEL IMPL, EVAL
2. do 5 better, not sure how, figure first

related work
- dep types
- occurrence types TR optimizer (no theoretical justifications)
- yes we did macros, yes that's been done (fisher, hermen)
- haskell

conclusion
- jsut one way of doing this, can reimagin typed racket, TC + elab
= stephen chang


regexP;
- 
- put the (list ...) back
