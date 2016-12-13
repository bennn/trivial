Project Structure
---

- `private/` Contains the implementation
- `scribblings/` Documentation sources
- `info.rkt` Racket package metadata
- `main.rkt` Package front-end, use with `(require trivial)`
- `no-colon.rkt` Alternate front-end, use with `(require trivial/no-colon)`
- `*.rkt` Front-ends for specific libraries
- `*/` Front-ends for the no-colon versions of specific libraries, like `(require format/no-colon)`


Note: transformations do not have access to type information because Typed
 Racket seems to keep type information in an external table.

Could try to import the table, but its probably not populated at the time.
If only types were stored as interned syntax properties!
