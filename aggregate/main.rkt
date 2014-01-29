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
#;(provide (contract-out
          [aggregator? (-> any/c boolean?)]
          [agg-val (-> aggregator? any)]
          [agg-step (-> aggregator? any/c aggregator?)]
          [agg-finish (-> aggregator? aggregator?)]))

; constructor wrappers
(provide (contract-out
          [-->count (->* () (integer? #:key (-> any/c integer?))
                         aggregator?)]
          [-->sum (->* () ((or/c number? void?) #:key (-> any/c number?))
                       aggregator?)]
          [-->min (->* () (any/c #:key (-> any/c any) #:< (-> any/c any/c boolean?) #:output (or/c 'from-key 'input))
                       aggregator?)]
          [-->max (->* () (any/c #:key (-> any/c any) #:> (-> any/c any/c boolean?) #:output (or/c 'from-key 'input))
                       aggregator?)]
          [-->mean (->* () ((or/c number? void?) #:key (-> any/c number?))
                        aggregator?)]
          [-->list (->* () (any/c #:key (-> any/c any) #:finish (-> list? any))
                        aggregator?)]))

; aggregate building operations + wrappers
(provide (contract-out
          [aggregate (->* (sequence?) ((listof aggregator?))
                          list?)]
          [group (->* (sequence?) (#:key (-> any/c any) #:aggregates (-> (listof aggregator?)))
                      (hash/c any/c list?))]
          [tally (->* (sequence?) (#:key (-> any/c any))
                      (hash/c any/c integer?))]
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

;; dispatch manually and fallback to generic.
;; testing shows 2x-5x performance increase
(define (dispatch-agg-val agg)
  (cond
    [(aggregate/list? agg) (agg-val/list agg)]
    [(aggregate/count? agg) (agg-val/count agg)]
    [(aggregate/sum? agg) (agg-val/sum agg)]
    [(aggregate/min? agg) (agg-val/min agg)]
    [(aggregate/max? agg) (agg-val/max agg)]
    [(aggregate/mean? agg) (agg-val/mean agg)]
    [else (agg-val agg)]))

(define (dispatch-agg-step agg x)
  (cond
    [(aggregate/list? agg) (agg-step/list agg x)]
    [(aggregate/count? agg) (agg-step/count agg x)]
    [(aggregate/sum? agg) (agg-step/sum agg x)]
    [(aggregate/min? agg) (agg-step/min agg x)]
    [(aggregate/max? agg) (agg-step/max agg x)]
    [(aggregate/mean? agg) (agg-step/mean agg x)]
    [else (agg-step agg x)]))

(define (dispatch-agg-finish agg)
  (cond
    [(aggregate/list? agg) (agg-finish/list agg)]
    [(aggregate/count? agg) (agg-finish/count agg)]
    [(aggregate/sum? agg) (agg-finish/sum agg)]
    [(aggregate/min? agg) (agg-finish/min agg)]
    [(aggregate/max? agg) (agg-finish/max agg)]
    [(aggregate/mean? agg) (agg-finish/mean agg)]
    [else (agg-finish agg)]))

;;;
;;; standard aggregates
;;;

;; Simulates SQL COUNT
(define (agg-val/count agg)
  (aggregate/count-value agg))

(define (agg-step/count agg x)
  (let* ([key (aggregate/count-key agg)]
         [unlocked (key x)]
         [old (aggregate/count-value agg)]
         [new (+ old unlocked)])
    (set-aggregate/count-value! agg new)
    agg))

(define (agg-finish/count agg)
  agg)

(struct aggregate/count ((value #:mutable) key)
  #:transparent
  #:methods gen:aggregator
  [(define agg-val agg-val/count)
   (define agg-step agg-step/count)
   (define agg-finish agg-finish/count)])

;; Simulates SQL SUM
(define (agg-val/sum agg)
  (aggregate/sum-value agg))

(define (agg-step/sum agg x)
  (let* ([key (aggregate/sum-key agg)]
         [unlocked (key x)]
         [old (aggregate/sum-value agg)]
         [new (if (void? old)
                  unlocked
                  (+ old unlocked))])
    (set-aggregate/sum-value! agg new)
    agg))

(define (agg-finish/sum agg)
  agg)

(struct aggregate/sum ((value #:mutable) key)
  #:transparent
  #:methods gen:aggregator
  [(define agg-val agg-val/sum)
   (define agg-step agg-step/sum)
   (define agg-finish agg-finish/sum)])

;; Simulates SQL MIN but with customisable less-than operator and
;; saves both the "full" item and key-extracted value on state change.
(define (agg-val/min agg)
  (if (equal? (aggregate/min-output agg) 'from-key)
      (aggregate/min-value agg)
      (aggregate/min-item agg)))

(define (agg-step/min agg x)
  (let* ([key (aggregate/min-key agg)]
         [unlocked (key x)]
         [<operator (aggregate/min-<operator agg)]
         [old (aggregate/min-value agg)])
    (when (or (void? old) (<operator unlocked old))
      (begin
        (set-aggregate/min-item! agg x)
        (set-aggregate/min-value! agg unlocked)
        agg))))

(define (agg-finish/min agg)
  agg)

(struct aggregate/min ((value #:mutable) key <operator output (item #:mutable))
  #:transparent
  #:methods gen:aggregator
  [(define agg-val agg-val/min)
   (define agg-step agg-step/min)
   (define agg-finish agg-finish/min)])

;; Simulates SQL MAX but with customisable greater-than operator and
;; saves both the "full" item and key-extracted value on state change.
(define (agg-val/max agg)
  (if (equal? (aggregate/max-output agg) 'from-key)
      (aggregate/max-value agg)
      (aggregate/max-item agg)))

(define (agg-step/max agg x)
  (let* ([key (aggregate/max-key agg)]
         [unlocked (key x)]
         [>operator (aggregate/max->operator agg)]
         [old (aggregate/max-value agg)])
    (when (or (void? old) (>operator unlocked old))
      (begin
        (set-aggregate/max-item! agg x)
        (set-aggregate/max-value! agg unlocked)
        agg))))

(define (agg-finish/max agg)
  agg)

(struct aggregate/max ((value #:mutable) key >operator output (item #:mutable))
  #:transparent
  #:methods gen:aggregator
  [(define agg-val agg-val/max)
   (define agg-step agg-step/max)
   (define agg-finish agg-finish/max)])

;; Simulates SQL AVG and calculates arithmetic mean.
(define (agg-val/mean agg)
  (aggregate/mean-value agg))

(define (agg-step/mean agg x)
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

(define (agg-finish/mean agg)
  (let* ([old (aggregate/mean-value agg)]
         [new (if (void? old)
                  (void)
                  (/ old (aggregate/mean-n agg)))])
    (set-aggregate/mean-value! agg new)
    agg))

(struct aggregate/mean ((value #:mutable) key (n #:mutable))
  #:transparent
  #:methods gen:aggregator
  [(define agg-val agg-val/mean)
   (define agg-step agg-step/mean)
   (define agg-finish agg-finish/mean)])

; list aggregate
(define (agg-val/list agg)
  (aggregate/list-value agg))

(define (agg-step/list agg x)
  (let* ([key (aggregate/list-key agg)]
         [unlocked (key x)]
         [old (aggregate/list-value agg)]
         [new (if (void? old)
                  (list unlocked)
                  (cons unlocked old))])
    (set-aggregate/list-value! agg new)
    agg))

(define (agg-finish/list agg)
  (let* ([finish (aggregate/list-finish agg)]
         [old (aggregate/list-value agg)]
         [new (if (void? old)
                  (void)
                  (finish old))])
    (set-aggregate/list-value! agg new)
    agg))

(struct aggregate/list ((value #:mutable) key finish)
  #:transparent
  #:methods gen:aggregator
  [(define agg-val agg-val/list)
   (define agg-step agg-step/list)
   (define agg-finish agg-finish/list)])

;;;
;;; constructor wrappers
;;;
(define (-->count (initial 0) #:key (key (const 1)))
  (aggregate/count initial key))

(define (-->sum (initial (void)) #:key (key identity))
  (aggregate/sum initial key))

(define (-->min (initial (void)) #:key (key identity) #:< (<operator <) #:output (output 'from-key))
  (aggregate/min initial key <operator output (void)))

(define (-->max (initial (void)) #:key (key identity) #:> (>operator >) #:output (output 'from-key))
  (aggregate/max initial key >operator output (void)))

(define (-->mean (initial (void)) #:key (key identity))
  (aggregate/mean initial key (if (void? initial) 0 1)))

(define (-->list (initial (void)) #:key (key identity) #:finish (finish reverse))
  (aggregate/list initial key finish))

;;;
;;; aggregate building operations + wrappers
;;;
; aggregate* - return the aggregates
; meant to be private, a user most probably just wants
; the values and will use aggregate
(define (aggregate* xs aggs)
  (for* ([x xs]
         [agg aggs])
    (dispatch-agg-step agg x))
  (for/list ([agg aggs])
    (dispatch-agg-finish agg)))

; aggregate - return the finished values
(define (aggregate xs (aggs (list (-->count))))
  (for/list ([agg (aggregate* xs aggs)])
    (dispatch-agg-val agg)))

; like we do for aggregate* and aggregate - we'll have a private
; implementation to return the aggregates and a user facing one
; to return the finalised values
(define (group* xs #:key (key identity) #:aggregates (aggregates (λ () (list (-->count)))))
  (let ([groups (make-hash)])
    (for ([x xs])
      (let* ([group-key (key x)]
             [group-value (hash-ref groups group-key #f)])
        (if group-value
            (for ([agg group-value])
              (dispatch-agg-step agg x))
            (let ([aggs (for/list ([agg (aggregates)])
                          (dispatch-agg-step agg x))])
              (hash-set! groups group-key aggs)))))
    ; visited everything - now finish the aggregates
    (for ([group-key (hash-keys groups)])
      (let ([aggs (hash-ref groups group-key)])
        (for ([agg aggs])
          (dispatch-agg-finish agg))))
    groups))

(define (group xs #:key (key identity) #:aggregates (aggregates (λ () (list (-->count)))))
  (define groups (group* xs #:key key #:aggregates aggregates))
  (for ([group-key (hash-keys groups)])
    (let ([aggs (hash-ref groups group-key)])
      (hash-set! groups group-key (map dispatch-agg-val aggs))))
  groups)

; like Mathematica tally
(define (tally xs #:key (key identity))
  (let ([groups (make-hash)])
    (for ([x xs])
      (let* ([group-key (key x)]
             [group-value (hash-ref groups group-key 0)])
        (hash-set! groups group-key (add1 group-value))))
    groups))

; like Mathematica GatherBy - but GatherBy returns just the values we
; return the keys and values
(define (gather-by xs #:key (key identity))
  (define grouped (group xs
                         #:key key
                         #:aggregates (thunk (list (-->list)))))
  (for ([group-key (hash-keys grouped)])
    (let ([agg-vals (hash-ref grouped group-key)])
      (hash-set! grouped group-key (first agg-vals))))
  grouped)

; like Mathematica GatherBy and just return the values in a list
(define (gather-by/values xs #:key (key identity))
  (hash-values (gather-by xs #:key key)))

