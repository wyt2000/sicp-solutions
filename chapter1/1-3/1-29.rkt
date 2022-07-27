#lang sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a (+ b 1e-6))			;In order to avoid the error of float number precision, we should let a > b + epsilon
      0					;Otherwise, the last term won't be add to sum.
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)

(define (Simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x (* 2.0 h)))
  (* (/ h 3.0)
     (+ (f a)
        (* 4.0 (sum f (+ a h) add-2h (- b h)))
        (* 2.0 (sum f (+ a (* 2.0 h)) add-2h (- b (* 2 h))))
        (f b))))

(Simpson-integral cube 0 1 100)
(Simpson-integral cube 0 1 1000)
