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

; group - like aggregate defaults to count
(check-equal? (group empty) (make-hash))
(check-equal? (group (range 10) #:key (const #t))
              (make-hash (list '(#t . (10)))))
(check-equal? (group (range 10) #:key even?)
              (make-hash (list '(#t . (5)) '(#f . (5)))))
(check-equal? (group (range 10) #:key even? #:aggregates (thunk (list (-->count) (-->sum))))
              (make-hash (list '(#t . (5 20)) '(#f . (5 25)))))
(check-equal? (group (range 10) #:key even? #:aggregates (thunk (list (-->list))))
              (make-hash (list '(#t . ((0 2 4 6 8))) '(#f . ((1 3 5 7 9))))))

; tally
(check-equal? (tally empty) (make-hash))
(check-equal? (tally (range 5))
              (make-hash (list '(0 . 1) '(1 . 1) '(2 . 1) '(3 . 1) '(4 . 1))))
(check-equal? (tally (range 10) #:key even?)
              (make-hash (list '(#t . 5) '(#f . 5))))
