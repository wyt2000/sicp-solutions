#lang sicp

(define (expmod base exp m)
  (define (square x) (* x x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 5))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  )

(define (search-for-primes from to)

  (define (search-for-primes-iter)
    (if (= (remainder from 2) 1) (timed-prime-test from))
    (search-for-primes (+ from 1) to))

  (if (< from to)
    (search-for-primes-iter)))

(search-for-primes 1000 10000)
(search-for-primes 10000 100000)
(search-for-primes 100000 1000000)
;When n is big enough, the speed rate is close to 1:2.

