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
;;; * standard aggregates
;;; * constructor wrappers
;;; * aggregate building operations + wrappers
;;; 

(require racket/generic)

; types and interfaces
(provide (contract-out
          [aggregator? (-> any/c boolean?)]
          [agg-val (-> aggregator? any)]
          [agg-step (-> aggregator? any/c void)]
          [agg-finish (-> aggregator? aggregator?)]))

; standard aggregates
(provide (contract-out
          [aggregate/min-item (-> aggregator? any)]
          [aggregate/max-item (-> aggregator? any)]))

; constructor wrappers
(provide (contract-out
          [-->count (->* () (any/c #:key (-> any/c integer?))
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
                          (listof aggregator?))]
          [aggregate/agg-val (-> sequence? (listof aggregator?) list?)]
          [aggregate/summary (-> sequence? (listof (list/c symbol? number?)))]
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
