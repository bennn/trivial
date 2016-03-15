trivial @ icfp 2016, hopefully
===

TODO
    "United States v. Hayes, 555 U.S. 415 (2009)")
>  (regexp-match #rx"(a*)b" "aab")
'("aab" "aa")
>  (regexp-match #rx"(a*)b" "ccc")
#false
-
-
Like Shakespeare's Portia, we understand that the phrase ``pound of flesh''
 says nothing about drawing blood and specialize accordingly.

---

1. Intro
   - Simple + Macros ~~~ Dependent
   - Obvious to programmer now obvious to type system
   - On the shoulders of Herman / Menier
   - Examples
2. Solution sketch
   - Key functions / metafunctions
   - Formulate requirements for all languages
3. Examples
   - Our implementation does X,Y,Z
   - Limitations
4. Implementation
   - How it works, quickly
   - Full impl. of format, addition
   - Partial impl. of regexp-match, vector-ref
5. Correctness
   - Desirable properties of implementation
   - General requirements
   - Open question: correct-by-construction
6. Related Work
   - Herman + Menier
   - Dependent Types
   - Hasochism
   - Do we need dependent types
7. Conclusion
   - idk


Maybe we'll finish under 12 pages.



JFP Guidelines
---

Functional Pearls: elegant, instructive, and fun essays on functional programming.

a full paper of at most 12 pages (6 pages for an Experience Report)
 in standard SIGPLAN conference format, including figures but excluding bibliography.

Formatting: Submissions must be in PDF format printable in black and white
 on US Letter sized paper and interpretable by Ghostscript.
Papers must adhere to the standard SIGPLAN conference format:
 two columns,
 nine-point font on a ten-point baseline,
 with columns 20pc (3.33in) wide and 54pc (9in) tall,
 with a column gutter of 2pc (0.33in).
A suitable document template for LaTeX is available at http://www.sigplan.org/Resources/Author/.


A Functional Pearl is an elegant essay about something related to functional programming. Examples include, but are not limited to:

- a new and thought-provoking way of looking at an old idea
- an instructive example of program calculation or proof
- a nifty presentation of an old or new data structure
- an interesting application of functional programming techniques
- a novel use or exposition of functional programming in the classroom

While pearls often demonstrate an idea through the development of a short
 program, there is no requirement or expectation that they do so.
Thus, they encompass the notions of theoretical and educational pearls.

Functional Pearls are valued as highly and judged as rigorously as
 ordinary papers, but using somewhat different criteria.
In particular, a pearl is not required to report original research, but,
 it should be concise, instructive, and entertaining.
Your pearl is likely to be rejected if your readers get bored,
 if the material gets too complicated,
 if too much specialized knowledge is needed,
 or if the writing is inelegant.

A submission you wish to have treated as a pearl must be marked as such
 on the submission web page, and should contain the words “Functional Pearl”
 somewhere in its title or subtitle.
These steps will alert reviewers to use the appropriate evaluation criteria.
Pearls will be combined with ordinary papers, however,
 for the purpose of computing the conference’s acceptance rate.



Overflow
---

This pearl describes a technique for extending a simple type system with
 a value-indexed form of polymorphism.
By analyzing the syntactic structure of values and partially evaluating
 constant expressions before typechecking, we specialize the types of functions
 like @racket[curry], @racket[first], and @racket[regexp-match] at their
 call-sites when possible.
Whereas the general type of @racket[curry] is @racket[(⊥ -> ⊤)],
 our system infers when it is safe to use a subtype instead.
For instance:

@racketblock[
 (curry (λ (x y) x))
]

generates the type constraint

@racketblock[
 curry : ((A B -> A) -> (A -> B -> A))
]

---

Readers with a phd in type theory may think: "these are just wannabe dependent types".
That is correct.
Readers with a phd in macrology may think: "these are just macros".
Also correct.
We think we have found a sweet spot, combining the basic features of macros
 to get the basic utility of dependent types.

Let the programmers judge.

---

also useful for untyped (goes wihtout saying?)

---

  A static type system is a compromise between rejecting all bad programs
   and approving all good programs, where @emph{bad} and @emph{good} are
   formalized in terms of a language's operational semantics.
  Consequently, every useful type system rejects some well-behaved programs
   and approves other programs that go wrong at runtime.
