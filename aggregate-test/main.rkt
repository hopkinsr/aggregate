#lang racket
(require rackunit)
(require "../aggregate/main.rkt")

; aggregate defaults to count
(check-equal? (aggregate empty) '(0))
(check-equal? (aggregate (range 10)) '(10))

; custom starting point for count
(check-equal? (aggregate empty (list (-->count))) '(0))
(check-equal? (aggregate empty (list (-->count 17))) '(17))
(check-equal? (aggregate (range 10) (list (-->count 17))) '(27))

; basic aggregates
(check-equal? (aggregate empty (list (-->min) (-->max) (-->count) (-->sum) (-->mean)))
              (list (void) (void) 0 (void) (void)))
(check-equal? (aggregate empty (list (-->min 1) (-->max 2) (-->count 3) (-->sum 4) (-->mean 5)))
              (list 1 2 3 4 5))
(check-equal? (aggregate (range 10) (list (-->min) (-->max) (-->count) (-->sum) (-->mean)))
              (list 0 9 10 45 9/2))

; aggregate/summary
(check-equal? (aggregate/summary empty)
              (list (list 'count 0)
                    (list 'min (void))
                    (list 'max (void))
                    (list 'mean (void))))
(check-equal? (aggregate/summary (range 10))
              (list (list 'count 10)
                    (list 'min 0)
                    (list 'max 9)
                    (list 'mean 9/2)))
