#lang racket
(require rackunit)
(require "../aggregate/main.rkt")

; tally
(check-equal? (tally empty) (make-hash))
(check-equal? (tally (range 5))
              (make-hash (list '(0 . 1) '(1 . 1) '(2 . 1) '(3 . 1) '(4 . 1))))
(check-equal? (tally (range 10) #:key even?)
              (make-hash (list '(#t . 5) '(#f . 5))))
(check-equal? (tally '(a a b c a b a))
              (make-hash '((a . 4) (b . 2) (c . 1))))

; gather-by
(check-equal? (gather-by empty) (make-hash))
(check-equal? (gather-by (range 10) #:key (const #t))
              (make-hash (list '(#t . (0 1 2 3 4 5 6 7 8 9)))))
(check-equal? (gather-by (range 10) #:key (λ (x) (modulo x 3)))
              (make-hash (list '(0 . (0 3 6 9)) '(1 . (1 4 7)) '(2 . (2 5 8)))))

; gather-by/values
(check-equal? (gather-by/values empty) empty)
(check-equal? (gather-by/values (range 10) #:key (const #t))
              (list (list 0 1 2 3 4 5 6 7 8 9)))
; gather-by/values doesn't guarantee the order - so we'll sort manually here
(check-equal? (sort (gather-by/values (range 10) #:key (λ (x) (modulo x 3)))
                     (λ (x y) (< (first x) (first y))))
              (list '(0 3 6 9) '(1 4 7) '(2 5 8)))
