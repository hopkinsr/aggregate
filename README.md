aggregate
=========

IMPORTANT: Currently in development. There will almost definitely be breakages
and out-of-date documentation before the first release of this as a proper
Racket package.

A Racket library which implements and provides a Racket generics interface
for SQL-style aggregate operations, as well as common operations to group
values Mathematica-style.

Appetisers
----------

  > (aggregate (range 10) (list (-->min) (-->max) (-->count) (-->sum) (-->mean)))
  
  '(0 9 10 45 9/2)

  > (group (range 10) #:key even? #:aggregates (thunk (list (-->min) (-->max) (-->count) (-->sum) (-->mean))))
  
  '#hash((#t . (0 8 5 20 4)) (#f . (1 9 5 25 5)))

  > (tally (range 10) #:key even?)
  
  '#hash((#t . 5) (#f . 5))

  > (gather-by (range 10) #:key (λ (x) (modulo x 3)))
  
  '#hash((2 . (2 5 8)) (1 . (1 4 7)) (0 . (0 3 6 9)))

  > (gather-by/values (range 10) #:key (λ (x) (modulo x 3)))
  
  '((2 5 8) (1 4 7) (0 3 6 9))

