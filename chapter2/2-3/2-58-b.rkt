#lang sicp

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(define (make-exponent base power)
  (cond ((=number? power 0) 1)
	((=number? power 1) base)
	(else (list base '** power))))

(define (op? symbol)
  (or (eq? symbol '+) (eq? symbol '*) (eq? symbol '**) (eq? symbol 'SUP)))

(define (get-op-precedence op)
  (cond ((eq? op '+) 0)
	((eq? op '*) 1)
	((eq? op '**) 2)
	((eq? op 'SUP) 3)
	(else (error "Operator Invaild!" op))))

(define (op< op1 op2)
  (< (get-op-precedence op1) (get-op-precedence op2)))

(define (min-precedence-op op1 op2)
  (cond ((not (op? op1)) (min-precedence-op op2 'SUP))
	((not (op? op2)) op1) 
	((op< op1 op2) op1)
	(else op2)))

(define (accumulate p zero seq)
  (if (null? seq)
      zero
      (p (car seq) (accumulate p zero (cdr seq)))))

(define (min-precedence-op-from-exp exp)
  (accumulate min-precedence-op 'SUP exp)) 

(define (split-prefix items delimiter)
  (if (or (null? items) (eq? (car items) delimiter))
      nil 
      (cons (car items) (split-prefix (cdr items) delimiter))))

(define (split-suffix items delimiter)
  (cond ((null? items) nil)
	((eq? (car items) delimiter) (cdr items))
	(else (split-suffix (cdr items) delimiter))))

(define (exponent? x) (and (pair? x) (eq? (min-precedence-op-from-exp x) '**)))

(define (base e)
  (define prefix (split-prefix e '**))
  (if (= (length prefix) 1)
      (car prefix)
      prefix))

(define (power e)
  (define suffix (split-suffix e '**))
  (if (= (length suffix) 1)
      (car suffix)
      suffix))

(define (sum? x) (and (pair? x) (eq? (min-precedence-op-from-exp x) '+)))

(define (addend s)
  (define prefix (split-prefix s '+))
  (if (= (length prefix) 1)
      (car prefix)
      prefix))

(define (augend s)
  (define suffix (split-suffix s '+))
  (if (= (length suffix) 1)
      (car suffix)
      suffix))

(define (product? x) (and (pair? x) (eq? (min-precedence-op-from-exp x) '*)))

(define (multiplier p)
  (define prefix (split-prefix p '*))
  (if (= (length prefix) 1)
      (car prefix)
      prefix))

(define (multiplicand p)
  (define suffix (split-suffix p '*))
  (if (= (length suffix) 1)
      (car suffix)
      suffix))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var)))
	((product? exp) (make-sum (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
				  (make-product (deriv (multiplier exp) var)
						(multiplicand exp))))
	((exponent? exp) (make-product 
			   (make-product (power exp)
				         (make-exponent (base exp)
						        (make-sum (power exp) -1)))
			   (deriv (base exp) var)))

	(else (error "unknown expression" exp))))

(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x + 3) 'x)
(deriv '(x * y * (x + 3)) 'x)
(deriv '((x * y) * (x + 3)) 'x)
(deriv '(x * (y * (x + 3))) 'x)


