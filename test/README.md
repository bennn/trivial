test
===

Root directory for `trivial` package test cases.

There should be 2 files for each module `M` in the parent directory.
- `M-pass.rkt` : tests exercising intended behavior
- `M-fail.rkt` : tests that should raise type errors

Run tests for any file with `raco test FILE.rkt`.
