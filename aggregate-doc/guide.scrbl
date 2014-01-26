#lang scribble/manual
@(require scribble/eval)

@title{aggregate guide}

@section{Welcome}

@section{Appetiser}

@examples[(require racket/list
                   racket/function
                   "../aggregate/main.rkt")
          (aggregate (range 10) (list (-->min) (-->max) (-->count) (-->sum) (-->mean)))
          (group (range 10) #:key even? #:aggregates (thunk (list (-->min) (-->max) (-->count) (-->sum) (-->mean))))
          (tally (range 10) #:key even?)
          (gather-by (range 10) #:key (λ (x) (modulo x 3)))
          (gather-by/values (range 10) #:key (λ (x) (modulo x 3)))]
