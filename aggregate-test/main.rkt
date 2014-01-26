#lang racket
(require rackunit)
(require "../aggregate/main.rkt")

(check-equal? (map agg-val (aggregate (range 10)))
              (list 10))
