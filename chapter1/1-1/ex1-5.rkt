; Applicative-order evaluation: program is always running because it firstly evaluate the argument (p), and it will call itself recursively.
; Normal-order evaluation: program return 0 because it firstly evalute the procedure, i.e. extend the function body, and it will evalute (= 0 0), which returns #t, then the if expression return 0.
#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
