#lang scribble/sigplan

@title{Implementation}

@; - syntax parse
@; - 
@; - identifier macros
@; - let-bindings, make=rename-transformer
@; - define-bindings, free-id-table
@; - local expand
@; - phasing (we have + at all levels)

@; We don't re-implement format or regexp,
@; but we do implement + and some vector operations, to go faster

