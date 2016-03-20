Project Structure
---

- `private/` Contains the implementation
- `scribblings/` Documentation sources
- `info.rkt` Racket package metadata
- `main.rkt` Package front-end, use with `(require trivial)`
- `no-colon.rkt` Alternate front-end, use with `(require trivial/no-colon)`
- `*.rkt` Front-ends for specific libraries
- `*/` Front-ends for the no-colon versions of specific libraries, like `(require format/no-colon)`

