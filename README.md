dependent
===

Typed Racket does not have dependent types, but [macros](http://www.greghendershott.com/fear-of-macros/) are a pretty close substitute.
This library implements a few standard library functions as macros to provide strong compile-time guarantees.

For example, `printf!` is a type-safe printer:

```
(require dependent/format)
(printf! "hello, ~a\n") ;; Arity mismatch: expected 1 argument, got nothing.
```


Install
---

From Github:

```
> git clone https://github.com/bennn/dependent
> raco pkg install ./dependent
```

From the Racket [package server](http://pkgs.racket-lang.org/):

```
> raco pkg install dependent
```

Use `(require dependent)` to import all bindings from this library.
Each file can be required individually, as in `(require dependent/format)`.


API
---

Note: these are __macros__, not functions.
Don't use them in higher-order positions.

Everything macro here is a front-end for a standard library function.
Check the [Racket docs](http://docs.racket-lang.org/) for basic type signatures & behavior.
This library's [Scribble docs](TODO) give full details on the static checks.

#### `dependent/format`
- `format`
- `printf`

#### `dependent/regexp`
- `regexp-match?`
- `regexp-match`

