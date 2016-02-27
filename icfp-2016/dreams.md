dreams
===

Ideas not-quite-related to the main points of the paper


Contextual Equivalence, with Macros
---

Suppose `transform E e_1 = e_2`.
High level, want `e_1` and `e_2` to be indistinguishable to functions.

```
    \forall C . C[E[e_1]] ~ C[e_2]
```

But they are definitely not indistinguishable to macros.

```
  (define-syntax (foiled-once stx)
    (if (eq? (syntax->datum stx)
             (syntax->datum e_1))
      (let loop () (loop))
      (void)))

  (define-syntax (foiled-again stx)
    (if (equal? (syntax->srcloc stx)
                (syntax->srcloc e_1))
      (let loop () (loop))
      (void)))
```

We could argue these are ill-behaved macros.
Is there a useful notion of "well behaved" macros that are "parametric" in
 their arguments?


#### Idea: Limit destructors

From Tony G: limit introspection to values.
 Parametric over expression ~ parametric over sub-expressions
 parametric over values ~ do whatever

Then I guess, `syntax-case` should be the only destructor
 and we need primitive "value recognizers" to know what subterms can be mushed.


#### Idea: Parametric

Leave C "fully expaneded". i.e. cannot inspect its hole.
So C[e_1} expands the same way as C[e_2}, though evaluation may be different.
But evaluation is basically what we want.

