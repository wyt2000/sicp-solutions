#lang sicp

; subsets(s) = union(subsets(s - first-elem), insert first-elem to each set of subsets(s - first-elem))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
