#lang racket
;;; aggregate
;;;
;;; A Racket library which implements and provides a Racket generics interface
;;; for SQL-style aggregate operations, as well as common operations to group
;;; values Mathematica-style.
;;;
;;; Order of the file
;;;
;;; * requires + provides
;;; * types and interfaces
;;; * interface helpers
;;; * standard aggregates
;;; * constructor wrappers
;;; * aggregate building operations + wrappers
;;; 

(require racket/generic)

; types and interfaces
(provide (contract-out
          [aggregator? (-> any/c boolean?)]
          [agg-val (-> aggregator? any)]
          [agg-step (-> aggregator? any/c aggregator?)]
          [agg-finish (-> aggregator? aggregator?)]))

; interface helpers
(provide (contract-out
          [agg-step/each (-> (listof aggregator?) any/c void)]
          [agg-step/list (-> (listof aggregator?) any/c (listof aggregator?))]
          [agg-finish/each (-> (listof aggregator?) void)]
          [agg-finish/list (-> (listof aggregator?) (listof aggregator?))]))

; standard aggregates
(provide (contract-out
          [aggregate/min-item (-> aggregator? any)]
          [aggregate/max-item (-> aggregator? any)]))

; constructor wrappers
(provide (contract-out
          [-->count (->* () (integer? #:key (-> any/c integer?))
                         aggregator?)]
          [-->sum (->* () (any/c #:key (-> any/c number?))
                       aggregator?)]
          [-->min (->* () (any/c #:key (-> any/c any) #:<operator (-> any/c boolean?))
                       aggregator?)]
          [-->max (->* () (any/c #:key (-> any/c any) #:>operator (-> any/c boolean?))
                       aggregator?)]
          [-->mean (->* () (any/c #:key (-> any/c number?))
                        aggregator?)]
          [-->list (->* () (any/c #:key (-> any/c any))
                        aggregator?)]))

; aggregate building operations + wrappers
;(provide aggregate)
(provide (contract-out
          [aggregate (->* (sequence?) ((listof aggregator?))
                          list?)]
          [aggregate/summary (-> sequence? (listof (list/c symbol? (or/c number? void?))))]
          [group (->* (sequence?) (#:key (-> any/c any) #:aggregates (-> (listof aggregator?)))
                      (hash/c any/c (listof aggregator?)))]
          [group/agg-val (->* (sequence?) (#:key (-> any/c any) #:aggregates (-> (listof aggregator?)))
                              (hash/c any/c any/c))]
          [tally (-> sequence? (hash/c any/c integer?))]
          [gather-by (->* (sequence?) (#:key (-> any/c any))
                          (hash/c any/c list?))]
          [gather-by/values (->* (sequence?) (#:key (-> any/c any))
                                 (listof list?))]))

;;;
;;; types and interfaces
;;;
(define-generics aggregator
  (agg-val aggregator)
  (agg-step aggregator x)
  (agg-finish aggregator))

;;;
;;; interface helpers
;;;
(define (agg-step/each aggs x)
  (for-each (λ (agg) (agg-step agg x))
            aggs))

(define (agg-step/list aggs x)
  (for/list ([agg aggs])
    (agg-step agg x)))

(define (agg-finish/each aggs)
  (for-each agg-finish aggs))

(define (agg-finish/list aggs)
  (for/list ([agg aggs])
    (agg-finish agg)))

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
            [new (+ old unlocked)])
       (set-aggregate/count-value! agg new)
       agg))
   
   (define (agg-finish agg)
     agg)])

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
       (set-aggregate/sum-value! agg new)
       agg))
   
   (define (agg-finish agg)
     agg)])

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
           (set-aggregate/min-value! agg unlocked)
           agg))))
   
   (define (agg-finish agg)
     agg)])

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
           (set-aggregate/max-value! agg unlocked)
           agg))))
   
   (define (agg-finish agg)
     agg)])

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
       (set-aggregate/mean-value! agg new-value)
       agg))
   
   (define (agg-finish agg)
     (let* ([old (aggregate/mean-value agg)]
            [new (if (void? old)
                     (void)
                     (/ old (aggregate/mean-n agg)))])
       (set-aggregate/mean-value! agg new)
       agg))])

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
       (set-aggregate/list-value! agg new)
       agg))
   
   (define (agg-finish agg)
     (let* ([old (aggregate/list-value agg)]
            [new (if (void? old)
                     (void)
                     (reverse old))])
       (set-aggregate/list-value! agg new)
       agg))])

;;;
;;; constructor wrappers
;;;
(define (-->count (initial 0) #:key (key (const 1)))
  (aggregate/count initial key))

(define (-->sum (initial (void)) #:key (key identity))
  (aggregate/sum initial key))

(define (-->min (initial (void)) #:key (key identity) #:<operator (<operator <))
  (aggregate/min initial key <operator (void)))

(define (-->max (initial (void)) #:key (key identity) #:>operator (>operator >))
  (aggregate/max initial key >operator (void)))

(define (-->mean (initial (void)) #:key (key identity))
  (aggregate/mean initial key (if (void? initial) 0 1)))

(define (-->list (initial (void)) #:key (key identity))
  (aggregate/list initial key))

;;;
;;; aggregate building operations + wrappers
;;;
; aggregate* - return the aggregates
; meant to be private, a user most probably just wants
; the values and will use aggregate
(define (aggregate* xs aggs)
  (for ([x xs])
    (agg-step/each aggs x))
  (agg-finish/list aggs))

; aggregate - return the finished values
(define (aggregate xs (aggs (list (-->count))))
  (for/list ([agg (aggregate* xs aggs)])
    (agg-val agg)))

(define (aggregate/summary xs)
  (define agg-summary (aggregate xs (list (-->count) (-->min) (-->max) (-->mean))))
  (match agg-summary
    [(list count min max mean)
     ; =>
     (list (list 'count count)
           (list 'min min)
           (list 'max max)
           (list 'mean mean))]))

(define (group xs #:key (key identity) #:aggregates (aggregates (λ () (list (-->count)))))
  (let ([groups (make-hash)])
    (for ([x xs])
      (let* ([group-key (key x)]
             [group-value (hash-ref groups group-key #f)])
        (if group-value
            (agg-step/each group-value x)
            (let ([aggs (agg-step/list (aggregates) x)])
              (hash-set! groups group-key aggs)))))
    ; visited everything - now finish the aggregates
    (for ([group-key (hash-keys groups)])
      (let ([aggs (hash-ref groups group-key)])
        (agg-finish/each aggs)))
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

; like Mathematica GatherBy - but GatherBy returns just the values we
; return the keys and values
(define (gather-by xs #:key (key identity))
  (define grouped (group/agg-val xs
                                 #:key key
                                 #:aggregates (thunk (list (-->list)))))
  (for ([group-key (hash-keys grouped)])
    (let ([agg-vals (hash-ref grouped group-key)])
      (hash-set! grouped group-key (first agg-vals))))
  grouped)

; like Mathematica GatherBy and just return the values in a list
(define (gather-by/values xs #:key (key identity))
  (define gathered (gather-by xs #:key key))
  (hash-values gathered))

