#lang racket
(require rackunit)
(require "../aggregate/main.rkt")

(check-equal? (aggregate (range 10))
              (list 10))
