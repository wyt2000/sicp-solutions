#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (car (cdr branch)))

(define (total-weight m)
  (if (pair? m)
      (+ (total-weight (branch-structure (left-branch m))) (total-weight (branch-structure (right-branch m))))
      m))

(define (mobile-balance? m)
  (if (pair? m)
      (let ((left-branch-structure (branch-structure (left-branch m)))
	    (right-branch-structure (branch-structure (right-branch m)))
            (left-branch-length (branch-length (left-branch m)))
	    (right-branch-length (branch-length (right-branch m))))
        (and (= (* left-branch-length (total-weight left-branch-structure)) (* right-branch-length (total-weight right-branch-structure)))
	     (mobile-balance? left-branch-structure)
	     (mobile-balance? right-branch-structure)))
      #t))


(define m (make-mobile (make-branch 10 2) (make-branch 5 4)))
(define n (make-mobile (make-branch 3 7) (make-branch 4 m)))

(mobile-balance? m)
(mobile-balance? n)

;If we change list to cons, we just need to modify right-branch and branch-structure.

