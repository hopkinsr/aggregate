aggregate
=========

A Racket library to implement and use SQL-style aggregate functions over a stream of values, as well as simple wrappers for the math/statistics modules for use in exploratory analysis.

Appetiser
---------

Lets get tasting (see manual.scrbl for more information). First, some standard definitions.

Examples:

    > (struct product (section name quantity))
    > (define shop (list (product 'food 'chocolate-bar 5)
                         (product 'literature 'magazine 4)
                         (product 'food 'bonbons 20)))

Some common statistics for a range of numbers.

Examples:

    > (define xs (range 10))
    > (aggregate xs)

    (list (aggregate/count 10 #<procedure:const>))

    > (aggregate xs (list (-->min)
                          (-->max)
                          (-->count)
                          (-->sum)))

    (list (aggregate/min 0 #<procedure:identity> #<procedure:<> 0)
          (aggregate/max 9 #<procedure:identity> #<procedure:>> 9)
          (aggregate/count 10 #<procedure:const>)
          (aggregate/sum 45 #<procedure:identity>))

    > (aggregate/summary xs)

    '((count 10) (min 0) (max 9) (mean 9/2))

Total count of products, and the minimum quantity.

Example:

    > (aggregate/agg-val shop
                         (list (-->count)
                               (-->min #:key product-quantity)))

    '(3 4)

First and last countries if we listed them lexicographically.

Example:

    > (aggregate/agg-val (list "Germany" "Argentina" "Japan" "Zimbabwe")
                         (list (-->min #:<operator string<?)
                               (-->max #:>operator string>?)))

    '("Argentina" "Zimbabwe")

Simple binning of numbers.

Example:

    > (group '(5 5 5 5 6 7 7 8 8 8))

    (hash 5 (list (aggregate/count 4 #<procedure:const>))
          6 (list (aggregate/count 1 #<procedure:const>))
          7 (list (aggregate/count 2 #<procedure:const>))
          8 (list (aggregate/count 3 #<procedure:const>)))

Slightly more advanced binning of numbers: we want to know how many are odd and even in a given range, along with the respective sums.

Example:

    > (group (range 10)
             #:key even?
             #:aggregates (thunk (list (-->count)
                                       (-->sum))))

    (hash #t (list (aggregate/count 5 #<procedure:const>)
                   (aggregate/sum 20 #<procedure:identity>))
          #f (list (aggregate/count 5 #<procedure:const>)
                   (aggregate/sum 25 #<procedure:identity>)))

They don’t have to be numbers. We want to know how many different products do we have in each section, along with how much stock.

Example:

    > (group shop
             #:key product-section
             #:aggregates (thunk (list (-->count)
                                       (-->sum #:key product-quantity))))

    (hash 'food (list (aggregate/count 2 #<procedure:const>)
                      (aggregate/sum 25 #<procedure:product-quantity>))
          'literature (list (aggregate/count 1 #<procedure:const>)
                            (aggregate/sum 4 #<procedure:product-quantity>)))
