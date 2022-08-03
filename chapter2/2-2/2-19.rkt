#lang sicp

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

(cc 100 (list 50 25 10 5 1)) 
(cc 100 (list 1 5 10 25 50)) ;no affect, because all conditions will be handled.
