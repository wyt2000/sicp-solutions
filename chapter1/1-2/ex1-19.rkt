#lang sicp
; Tpq(Tpq(a,b)) = Tpq(bq + aq + ap, bp + aq) = ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p, (bp + aq)p + (bq + aq + ap)q
;	        = (bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2, bp^2 + apq + bq^2 + aq^2 + apq)
;		= (ap^2 + bq^2 + 2aq^2 + 2apq + 2bpq, bp^2 + bq^2 + aq^2 + 2apq)
;		= ((p^2 + 2q^2 + 2pq)a + (q^2 + 2pq)b, (q^2 + 2pq)a + (p^2 + q^2)b)
; => p' = p^2 + q^2, q' = q^2 + 2pq

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
		   (+ (* p p) (* q q))
		   (+ (* q q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
(fib 100)
