#lang racket
;;; aggregate
;;;
;;; A Racket library to implement and use SQL-style aggregate functions over
;;; a stream of values, as well as simple wrappers for the math/statistics
;;; modules for use in exploratory analysis.
;;;
;;; Order of the file
;;;
;;; * requires + provides
;;; * types and interfaces
;;; * standard aggregates
;;; * constructor wrappers
;;; * aggregate building operations + wrappers
;;; * math/statistics wrappers
;;; * rackunit tests
;;; 

(require racket/generic)
(require math/statistics)

; types and interfaces
(provide aggregator?
         agg-val
         agg-step
         agg-finish)

; standard aggregates
(provide aggregate/min-item
         aggregate/max-item)

; constructor wrappers
(provide -->count
         -->sum
         -->min
         -->max
         -->mean
         -->list)

; aggregate building operations + wrappers
(provide aggregate
         aggregate/agg-val
         aggregate/summary
         group
         group/agg-val
         tally)

; math/statistics wrappers
(provide statistics-summary
         statistics-extended-summary
         collect-statistics
         collect-statistics/summary
         collect-statistics/extended-summary)

;;;
;;; types and interfaces
;;;
(define-generics aggregator
  (agg-val aggregator)
  (agg-step aggregator x)
  (agg-finish aggregator))

;;;
;;; standard aggregates
;;;

;; Simulates SQL COUNT.
(struct aggregate/count ((value #:mutable) key)
  #:transparent
  #:methods gen:aggregator
  [(define (agg-val agg)
     (aggregate/count-value agg))
   
   (define (agg-step agg x)
     (let* ([key (aggregate/count-key agg)]
            [unlocked (key x)]
            [old (aggregate/count-value agg)]
            [new (if (void? old)
                     unlocked
                     (+ old unlocked))])
       (set-aggregate/count-value! agg new)))
   
   (define (agg-finish agg)
     (agg-val agg))])

;; Simulates SQL SUM
(struct aggregate/sum ((value #:mutable) key)
  #:transparent
  #:methods gen:aggregator
  [(define (agg-val agg)
     (aggregate/sum-value agg))
   
   (define (agg-step agg x)
     (let* ([key (aggregate/sum-key agg)]
            [unlocked (key x)]
            [old (aggregate/sum-value agg)]
            [new (if (void? old)
                     unlocked
                     (+ old unlocked))])
       (set-aggregate/sum-value! agg new)))
   
   (define (agg-finish agg)
     (agg-val agg))])

;; Simulates SQL MIN but with customisable less-than operator and
;; saves both the "full" item and key-extracted value on state change.
(struct aggregate/min ((value #:mutable) key <operator (item #:mutable))
  #:transparent
  #:methods gen:aggregator
  [(define (agg-val agg)
     (aggregate/min-value agg))
   
   (define (agg-step agg x)
     (let* ([key (aggregate/min-key agg)]
            [unlocked (key x)]
            [<operator (aggregate/min-<operator agg)]
            [old (aggregate/min-value agg)])
       (when (or (void? old) (<operator unlocked old))
         (begin
           (set-aggregate/min-item! agg x)
           (set-aggregate/min-value! agg unlocked)))))
   
   (define (agg-finish agg)
     (agg-val agg))])

;; Simulates SQL MAX but with customisable greater-than operator and
;; saves both the "full" item and key-extracted value on state change.
(struct aggregate/max ((value #:mutable) key >operator (item #:mutable))
  #:transparent
  #:methods gen:aggregator
  [(define (agg-val agg)
     (aggregate/max-value agg))
   
   (define (agg-step agg x)
     (let* ([key (aggregate/max-key agg)]
            [unlocked (key x)]
            [>operator (aggregate/max->operator agg)]
            [old (aggregate/max-value agg)])
       (when (or (void? old) (>operator unlocked old))
         (begin
           (set-aggregate/max-item! agg x)
           (set-aggregate/max-value! agg unlocked)))))
   
   (define (agg-finish agg)
     (agg-val agg))])

;; Simulates SQL AVG and calculates arithmetic mean.
(struct aggregate/mean ((value #:mutable) key (n #:mutable))
  #:transparent
  #:methods gen:aggregator
  [(define (agg-val agg)
     (aggregate/mean-value agg))
   
   (define (agg-step agg x)
     (let* ([key (aggregate/mean-key agg)]
            [unlocked (key x)]
            [old-n (aggregate/mean-n agg)]
            [old-value (aggregate/mean-value agg)]
            [new-value (if (void? old-value)
                           unlocked
                           (+ old-value unlocked))])
       (set-aggregate/mean-n! agg (add1 old-n))
       (set-aggregate/mean-value! agg new-value)))
   
   (define (agg-finish agg)
     (let* ([old (aggregate/mean-value agg)]
            [new (if (void? old)
                     (void)
                     (/ old (aggregate/mean-n agg)))])
       (set-aggregate/mean-value! agg new)
       (agg-val agg)))])

(struct aggregate/list ((value #:mutable) key)
  #:transparent
  #:methods gen:aggregator
  [(define (agg-val agg)
     (aggregate/list-value agg))
   
   (define (agg-step agg x)
     (let* ([key (aggregate/list-key agg)]
            [unlocked (key x)]
            [old (aggregate/list-value agg)]
            [new (if (void? old)
                     (list unlocked)
                     (cons unlocked old))])
       (set-aggregate/list-value! agg new)))
   
   (define (agg-finish agg)
     (let* ([old (aggregate/list-value agg)]
            [new (if (void? old)
                     (void)
                     (reverse old))])
       (set-aggregate/list-value! agg new)
       (agg-val agg)))])

;;;
;;; constructor wrappers
;;;
(define (-->count (initial (void)) #:key (key (const 1)))
  (aggregate/count initial key))

(define (-->sum (initial (void)) #:key (key identity))
  (aggregate/sum initial key))

(define (-->min (initial (void)) #:key (key identity) #:<operator (<operator <))
  (aggregate/min initial key <operator (void)))

(define (-->max (initial (void)) #:key (key identity) #:>operator (>operator >))
  (aggregate/max initial key >operator (void)))

(define (-->mean (initial (void)) #:key (key identity))
  (aggregate/mean initial key 0))

(define (-->list (initial (void)) #:key (key identity))
  (aggregate/list initial key))

;;;
;;; aggregate building operations + wrappers
;;;
(define (aggregate xs (aggs (list (-->count))))
  (for ([x xs])
    (for ([agg aggs])
      (agg-step agg x)))
  (for/list ([agg aggs])
    (agg-finish agg)
    agg))

; Helper to aggregate a sequence and just return the aggregated values
; and not the structs themselves.
(define (aggregate/agg-val xs aggs)
  (map agg-val (aggregate xs aggs)))

(define (aggregate/summary xs)
  (define agg-summary (aggregate xs (list (-->count) (-->min) (-->max) (-->mean))))
  (match agg-summary
    [(list count min max mean)
     ; =>
     (list (list 'count (agg-val count))
           (list 'min (agg-val min))
           (list 'max (agg-val max))
           (list 'mean (agg-val mean)))]))

(define (group xs #:key (key identity) #:aggregates (aggregates (λ () (list (-->count)))))
  (define (update-aggs aggs x)
    (for ([agg aggs])
      (agg-step agg x))
    aggs)
  
  (define (finish-aggs aggs)
    (for ([agg aggs])
      (agg-finish agg))
    aggs)
  
  (let ([groups (make-hash)])
    (for ([x xs])
      (let* ([group-key (key x)]
             [group-value (hash-ref groups group-key #f)])
        (if group-value
            (update-aggs group-value x)
            (begin
              (let ([aggs (update-aggs (aggregates) x)])
                (hash-set! groups group-key aggs))))))
    ; visited everything - now finish the aggregates
    (for ([group-key (hash-keys groups)])
      (let ([aggs (hash-ref groups group-key)])
        (finish-aggs aggs)))
    groups))

(define (group/agg-val xs #:key (key identity) #:aggregates (aggregates (λ () (list (-->count)))))
  (define grouped (group xs #:key key #:aggregates aggregates))
  (for ([group-key (hash-keys grouped)])
    (let ([aggs (hash-ref grouped group-key)])
      (hash-set! grouped group-key (map agg-val aggs))))
  grouped)

; like Mathematica tally
(define (tally xs #:key (key identity))
  (define grouped (group/agg-val xs #:key key #:aggregates (λ () (list (-->count)))))
  (for ([group-key (hash-keys grouped)])
    (let ([agg-vals (hash-ref grouped group-key)])
      (hash-set! grouped group-key (first agg-vals))))
  grouped)

;;;
;;; math/statistics wrappers
;;;

;; Returns a summary containing the count, min, max and mean.
; (statistics-summary statistics) -> (list (list symbol? flonum?) ...)
(define (statistics-summary s)
  (list (list 'count (statistics-count s))
        (list 'min (statistics-min s))
        (list 'max (statistics-max s))
        (list 'mean (statistics-mean s))))

;; Returns an extended summary containing the standard summary
;; (statistics-summary) as well as range, stddev, variance, skewness
;; and kurtosis.
; (statistics-summary statistics) -> (list (list symbol? flonum?) ...)
(define (statistics-extended-summary s)
  (append (statistics-summary s)
          (list (list 'range (statistics-range s))
                (list 'stddev (statistics-stddev s))
                (list 'variance (statistics-variance s))
                (list 'skewness (statistics-skewness s))
                (list 'kurtosis (statistics-kurtosis s)))))

;; Calculates statistics after visiting all of the values in xs
; (collect-statistics sequence) -> statistics
(define (collect-statistics xs)
  (update-statistics* empty-statistics xs))

;; Calculates statistics after visiting all of the values in xs and returns
;; a basic summary. See statistics-summary.
; (collect-statistics/summary sequence) -> (list (list symbol? flonum?) ...)
(define (collect-statistics/summary xs)
  (define s (collect-statistics xs))
  (statistics-summary s))

;; Calculates statistics after visiting all of the values in xs and returns
;; an extended summary. See statistics-extended-summary
; (collect-statistics/extended-summary sequence) -> (list (list symbol? flonum?) ...)
(define (collect-statistics/extended-summary xs)
  (define s (collect-statistics xs))
  (statistics-extended-summary s))

;;;
;;; rackunit tests
;;;
(module+ test
  (require rackunit)
  (require math/statistics)
  
  ; helpers for testing based on standard functions provided by racket
  (define (check-aggregate xs agg standard-function)
    (check-equal? (aggregate/agg-val xs (list (agg)))
                  (list (standard-function xs))))
  
  (define (check-aggregate/apply xs agg standard-function)
    (check-equal? (aggregate/agg-val xs (list (agg)))
                  (list (apply standard-function xs))))
  
  (define xs (range 100))
  
  (check-aggregate xs -->count length)
  (check-aggregate/apply xs -->sum +)
  (check-aggregate/apply xs -->min min)
  (check-aggregate/apply xs -->max max)
  (check-aggregate xs -->mean mean)
  
  (check-equal? (aggregate/agg-val (range 10)
                                   (list (-->min) (-->max) (-->count) (-->sum) (-->mean)))
                (list 0 9 10 45 9/2))
  
  (check-equal? (group/agg-val (range 10)
                               #:key (const #t)
                               #:aggregates (thunk (list (-->min) (-->max) (-->count) (-->sum) (-->mean))))
                (make-hash (list '(#t . (0 9 10 45 9/2)))))
  
  (check-equal? (group/agg-val (range 10)
                               #:key even?
                               #:aggregates (thunk (list (-->count)
                                                         (-->sum))))
                (make-hash (list '(#t . (5 20)) '(#f . (5 25)))))
  
  (check-equal? (group/agg-val (range 10)
                               #:key even?
                               #:aggregates (thunk (list (-->count)
                                                         (-->list))))
                (make-hash (list '(#t . (5 (0 2 4 6 8)))
                                 '(#f . (5 (1 3 5 7 9))))))
  
  (check-equal? (tally (range 10)
                       #:key even?)
                (make-hash (list '(#t . 5) '(#f . 5))))
  (check-equal? (tally '(5 5 5 5 6 7 7 8 8 8))
                (make-hash (list '(5 . 4) '(6 . 1) '(7 . 2) '(8 . 3)))))
