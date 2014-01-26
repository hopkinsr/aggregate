#lang scribble/manual
@(require scribble/eval)
@(require racket/sandbox)
@(require "aggregate.rkt")

@(define my-eval (make-evaluator 'racket
                                 (begin
                                   (sandbox-output 'string)
                                   (sandbox-error-output 'string))
                                 #:requires (list "aggregate.rkt"
                                                  'math/statistics)))

@title{aggregate}

@margin-note{IMPORTANT: Currently in development. There may be breakages before the
             first release of this as a proper Racket 6 package. This documentation may
             not be up-to-date with respect to the source too during development.}

A Racket library to implement and use SQL-style aggregate functions over
a stream of values, as well as simple wrappers for the math/statistics
modules for use in exploratory analysis.

@section{Appetiser}

Lets get tasting. First, some standard definitions.

@examples[#:eval my-eval
                 (struct product (section name quantity))
                 (define shop (list (product 'food 'chocolate-bar 5)
                                    (product 'literature 'magazine 4)
                                    (product 'food 'bonbons 20)))]

@itemlist[
          @item{Some common statistics for a range of numbers.
                @examples[#:eval my-eval
                                 (define xs (range 10))
                                 (aggregate xs)
                                 (aggregate xs (list (-->min) (-->max) (-->count) (-->sum)))
                                 (aggregate/summary xs)]}
           @item{Total count of products, and the minimum quantity.
                 @examples[#:eval my-eval
                                  (aggregate/agg-val shop
                                                     (list (-->count)
                                                           (-->min #:key product-quantity)))]}
           @item{First and last countries if we listed them lexicographically.
                 @examples[#:eval my-eval
                                  (aggregate/agg-val (list "Germany" "Argentina" "Japan" "Zimbabwe")
                                                     (list (-->min #:<operator string<?)
                                                           (-->max #:>operator string>?)))]}
           @item{
                 Simple binning of numbers.
                 @examples[#:eval my-eval
                                  (group '(5 5 5 5 6 7 7 8 8 8))]}
           
           @item{Slightly more advanced binning of numbers: we want to know how many
                 are odd and even in a given range, along with the respective sums
                 or even the actual values.
                 
                 @examples[#:eval my-eval
                                  (group/agg-val (range 10)
                                                 #:key even?
                                                 #:aggregates (thunk (list (-->count)
                                                                           (-->sum))))]
                 }
           @examples[#:eval my-eval
                            (group/agg-val (range 10)
                                           #:key even?
                                           #:aggregates (thunk (list (-->count)
                                                                     (-->list))))]
           @item{They don't have to be numbers. We want to know how many different products
                 do we have in each section, along with how much stock.
                 
                 @examples[#:eval my-eval
                                  (group shop
                                         #:key product-section
                                         #:aggregates (thunk (list (-->count)
                                                                   (-->sum #:key product-quantity))))]
                 }
                  ]

@section{SQL-style aggregates}

For example, the following SQL query

@verbatim{
          SELECT COUNT(*), MIN(n), MAX(n), AVG(n)
          FROM PositiveIntegers
          WHERE n BETWEEN 0 AND 9}

will produce a single row:

@verbatim{
          COUNT(*)  MIN(n)    MAX(n)    AVG(n)
          10        0         9         4.5}

This library provides basic aggregate functions to mirror the SQL equivalents
such as MIN, MAX, COUNT, etc, as well as provide a framework for applications
to implement their own aggregate functions and be treated as first-class
citizens along with the ones provided.

Specifically, it uses Racket's generics facility so all aggregators, provided
or custom, must implement the aggregator interface.

@section{The aggregator interface}

Each aggregator must implement the aggregator interface. In Racket this is
defined as

@codeblock{(define-generics aggregator
           (agg-val aggregator)
           (agg-step aggregator x)
           (agg-finish aggregator))}

@itemlist[
          @item{agg-val
                
                An aggregator must produce a value after visiting the whole stream.
                In the SQL example above, the MAX aggregator produced the value 9
                for all the positive integers between 0 and 9.
                
                This method must return the correct value after receiving all
                necessary values via agg-step and having agg-finish applied
                on completion.
                
                It can return a "current state" if applied part-way through a stream,
                for example, or after visiting the whole stream without applying
                agg-finish. In this case the answer may or may not be correct, may be
                approximate or even be completely wrong.}
           @item{agg-step
                 
                 Each aggregate must have a way of obtaining new information so it
                 can transition from one state to the next. It does not have to
                 change state after each call to agg-step.
                 
                 e.g. In the SQL example	above, the state of the MIN aggregate does
                 not change after 0 even though it received all of the values.}
           @item{agg-finish
                 
                 After receiving all of the values from the stream through agg-step,
                 the aggregate may need to perform a final calculation to produce
                 the correct value from agg-val.
                 
                 For simple aggregates such as MIN, MAX, COUNT etc, this is not needed
                 and they do nothing. More complex aggregates, however, might need to
                 perform extra calculations and they must do it here.
                 
                 The return value is that of agg-val.}]

@section{Provided aggregates}

The provided aggregates, although implemented as structs are, not constructed
through the standard constructor: they have a corresponding wrapper to ease
use. Just remember, if you are playing in the REPL the aggregates will print
with the struct name.

You will typically interface with the aggregates through

@itemlist[
          @item{these constructor wrappers;}
           @item{the standard aggregator interface (e.g. @code{agg-val}); or}
           @item{the provided aggregate builders which process a stream of values to obtain
                 final aggregate results (e.g. @code{aggregate}.}]

The naming convention for aggregates are to name the

@itemlist[
          @item{aggregate struct in the form @code{aggregate/operation}; and the}
           @item{constructor wrapper in the form @code{-->operation}.}]

Each provided aggregate constructor wrapper (and aggregate struct) also takes
a "key" which is applied to the input of @code{agg-step}. The result of applying this
key, and not the input value, is used for the transition (if any).

This simulates passing an expression to an aggregate function in SQL.

i.e. in SQL instead of MIN(<the-whole-row>) it is MIN(column_name) for example.

Likewise, instead of @code{(agg-step any-racket-value)}, conceptually* it will be
@code{(agg-step (key any-racket-value))}. As you might not need a key, such as
aggregating a simple list of numbers, the default key is @code{identity}.

* not really, see @code{-->min} and @code{-->max}.

The key is specified using the keyword argument @code{#:key}.

@section{Simple shop example and description}

For example, you might own a "poundshop" where every item is £1, and have
written a simple application to keep track of stock. See the appetisers
for the definition of this shop.

@examples[#:eval my-eval
                 shop]

Now you want to have a quick overview of your shop, so we will aggregate it

@examples[#:eval my-eval
                 (define shop-summary (aggregate shop
                                                 (list (-->count)
                                                       (-->min #:key product-quantity))))
                 shop-summary]

Ignore the funny looking things like #<procedure> for now, we'll come back to
it later. In short, they allow you to customise the behaviour of the aggregates.

The produced result says our shop has 3 products, and that the smallest
quantity of a product we have in stock is 4.

This is good, if our summary produced a quantity of 0 we might want to run
another report to find all products needing stock. However, as an extension to
the SQL-style MIN and MAX, whenever the state changes (which is based from the
key) we also save the given value. Note though that this ignores ties, and only
the "first" value of a tie is kept.

This means we no longer need to run another report and can get the information
from the summary

@examples[#:eval my-eval
                 (aggregate/min-item (second shop-summary))
                 (product-name (aggregate/min-item (second shop-summary)))]

@section{Provided aggregate constructor wrappers}

Each constructor wrapper is of the form

@verbatim{(-->operation initial-value #:key key)}

where @code{initial-value} and @code{key} are optional and defaults to @code{(void)}
and @code{identity} respectively. 

The aggregators know that @code{(void)} can be the initial value and act accordingly.

e.g. @code{-->sum} does not perform @code{(+ current x)} for the 1st state change as
@code{current} will be @code{(void)}.

Other constructor wrappers might have extra functionality and they are specified
as keyword arguments.

* @code{-->count} (aggregate/count)

Simulates the SQL COUNT aggregate.

* @code{-->sum} (aggregate/sum)

Simulates the SQL SUM aggregate.

* @code{-->min} (aggregate/min)

Simulates the SQL MIN aggregate, but also saves the value when the
state changes.

The less than operator is customisable and is specified using
@code{#:<operator}. The default is @code{<}.

The input to the operator is the current state and the result of
applying the key to the given value from @code{agg-step}.

e.g. find the minimum string lexicographically according to @code{string<?}

@code{(-->min #:<operator string<?)}

* @code{-->max} (aggregate/max)

Simulates the SQL MAX aggregate, but also saves the value when the
state changes.

The greater than operator is customisable and is specified using
@code{#:>operator}. The default is @code{>}.

The input to the operator is the current state and the result of
applying the key to the given value from @code{agg-step}.

e.g. find the minimum string lexicographically according to @code{string>?}

@code{(-->max #:operator string>?)}

* @code{-->mean} (aggregate/mean)

Simulates the SQL AVG aggregate and calculates the arithmetic mean.

This uses Rackets full numeric tower so the result will stay as
exact/inexact as the input.

If you would like a quicker way of calculating the mean consider using
the math/statistics library or the helpers provided here based on the
statistics calculators as they use flonums instead.

@section{Aggregate building operations}

The aggregate building operations can take any stream of values, and not just
a list. Basically, if it can be iterated with @code{for}, you can aggregate it.

* @code{(aggregate xs aggregates)} -> (listof aggregator?)

Visits each value from xs and passes it through each of the given
aggregates. On completion, each aggregate is then finalized with
@code{agg-finish}.

A new list containing the given aggregates with up-to-date values
is returned.

@examples[#:eval my-eval
                 (define agg-summary (aggregate (range 10)
                                                (list (-->count)
                                                      (-->sum))))
                 agg-summary
                 (map agg-val agg-summary)]

* @code{(aggregate/summary xs)} -> (listof (listof symbol? number?) ...)

Calls aggregate with some common aggregates, then returns a simple summary.

e.g. aggregate a list. note the exact 4.5.
@examples[#:eval my-eval
                 (aggregate/summary (range 10))]

e.g. aggregate a vector
@examples[#:eval my-eval
                 (aggregate/summary #(0 1 2 3 4 5 6 7 8 9))]

The format of the returned list is the same as the statistics summary
functions - but the values here adhere to @code{number?} and not @code{flonum?}
which is used by statistics.

* @code{(group xs #:key procedure? #:aggregates procedure?) -> (hash v aggregates ...) }

Where as @code{aggregate} is used to simulate the SQL aggregate functionality
by pumping in values with @code{agg-step} for a whole sequence, @code{group}
simulates the SQL GROUP BY functionality.

@code{group} takes a sequence and performs a grouping. Each produced group
will be an entry (key) in the returned @code{hash} instance. The corresponding
value will be the specified aggregates which will be up-to-date.

In very-pseudo-SQL, it would look something like

@verbatim{
          SELECT key(row), agg1(row), agg2(row), ... aggn(row)
          FROM xs
          GROUP BY key(row)}

@examples[#:eval my-eval
                 (group (range 10)
                        #:key even?
                        #:aggregates (thunk (list (-->count)
                                                  (-->sum))))]

* @code{(group/agg-val xs #: key #:aggregates)}

A wrapper for @code{group} but only returns the final aggregate values and not
the aggregates themselves.

@examples[#:eval my-eval
                 (group/agg-val (range 10)
                                #:key even?
                                #:aggregates (thunk (list (-->count)
                                                          (-->sum))))]

* @code{(tally xs #:key)}

@code{group} with the default values can perform simple binning to say
how many times a group appears but returns the aggregates themselves. 
@code{tally} does the same grouping and only returns the final counts.

@examples[#:eval my-eval
                 (tally '(5 5 5 5 6 7 7 8 8 8))]

@examples[#:eval my-eval
                 (tally (range 10)
                        #:key even?)]

@section{math/statistics wrappers}

Many of these wrappers are intended mainly for use at a REPL or for quick
exploratory analysis and require the math/statistics library.

The summary functions are intended to have the same format as
@code{aggregate/summary}.

* (statistics-summary statistics) -> (listof (listof symbol? flonum?) ...)

Returns basic statistical information from the given statistics.

@examples[#:eval my-eval
                 (statistics-summary empty-statistics)]

* @code{(statistics-extended-summary statistics)} -> (listof (listof symbol? flonum?) ...)

The same as statistics-summary but contains more statistical
information.

@examples[#:eval my-eval
                 (statistics-extended-summary empty-statistics)]

* @code{(collect-statistics xs)} -> statistics

Returns up-to-date statistics after visiting each value from xs. The
returned instance can be then passed to the math/statistics procedures
such as @code{statistics-mean}, or to @code{statistics-summary} etc.

* @code{(collect-statistics/summary xs)} -> (listof (listof symbol? flonum?) ...)

Collects up-to-date statistics for xs then returns a list containing
a basic summary of the values.

Also see @code{statistics-summary}.

Note the inexact 4.5.

@examples[#:eval my-eval
                 (collect-statistics/summary (range 10))]

* @code{(collect-statistics/extended-summary xs)} -> (listof (listof symbol? flonum?) ...)

* Performs the same duty as @code{collect-statistics/summary} but contains
more statistical information in the summary.

Also see @code{statistics-extended-summary}.

@section{Examples}

A simple summary example using the numbers 0-9 (inclusive).

Here, we want the count, minimum, maximum, maximum of the provided numbers
and 15, sum and the mean.

@examples[#:eval my-eval
                 (define summary (aggregate (range 10)
                                            (list (-->count)
                                                  (-->min)
                                                  (-->max)
                                                  (-->max 15)
                                                  (-->sum)
                                                  (-->mean))))
                 (map agg-val summary)]

@section{Requirements}

Currently playing around with this in Racket 5.3.6, however, this is intended
to be released as a proper Racket 6 package later on.
