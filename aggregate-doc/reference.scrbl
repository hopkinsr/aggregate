#lang scribble/manual
@(require (for-label racket
                     "../aggregate/base.rkt"))
@title{aggregate reference}

@section{Low-level functionality}

The Racket generics interface is intended for low-level interaction and
developers implementing their own aggregators. They are not intended
for everyday use by end users.

@defmodule[aggregate/base]

@defproc[(aggregator? [v any/c])
         boolean?]{
Returns @racket[#t] if @racket[v] is an aggregator, @racket[#f] otherwise.

An aggregator is a Racket struct which implements the @racket[gen:aggregator]
generic interface by providing
its own implementation for @racket[agg-val], @racket[agg-step] and
@racket[agg-finish].
}

@defproc[(agg-val [agg aggregator?])
         any/c]{
Returns the current value of @racket[agg]. This is only guaranteed to be
accurate if @racket[agg-finish] has been called and there have been no
further calls to @racket[agg-step].}

@defproc[(agg-step [agg aggregator?] [v any/c])
         aggregator?]{
Performs the appropriate aggregate operation on the current state of
@racket[agg] with the given value @racket[v]. The aggregate operation is
defined by @racket[agg].}

@defproc[(agg-finish [agg aggregator?])
         aggregator?]{
Performs any necessary operations for @racket[agg-val] to return an accurate
value.}

@section{Core building operations}
The core aggregate building operations deal with the low-level aggregator
functionality and will ensure the final results are accurate. As they use
the generic aggregator interface they will automatically use any new
aggregators passed by the end user. These building operations are intended
to be used by end users directly or by wrapper functions implementing higher
level functionality.