#lang sicp

(define (accumulate p zero seq)
  (if (null? seq)
      zero
      (p (car seq) (accumulate p zero (cdr seq)))))

(define (filter sat? items)
  (cond ((null? items) nil)
	((sat? (car items)) (cons (car items) (filter sat? (cdr items))))
	(else (filter sat? (cdr items)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval i j)
  (if (> i j)
      nil
      (cons i (enumerate-interval (+ i 1) j))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;we use row position from 8th col to 1st col: (list (list (1 8) (4 7) (5 6) (3 5) (2 4)...) (list...)) as positions


(define empty-board (list nil))

(define (adjoin-position new-row k rest-of-queens)
  (map (lambda (queen-list)
	 (if (null? queen-list)
	     (cons new-row k)
	     (cons (cons new-row k) queen-list)))
       rest-of-queens))

(adjoin-position 1 1 nil)

(define (sat-list? sat? items)
  (accumulate (lambda (x y) (and x y)) #t (map sat? items)))

(define (safe? k positions)
  (define kth-queen (car positions))
  (sat-list? (lambda (other-queen)
	       (and (not (= (car kth-queen) (car other-queen)))
		    (not (= (cdr kth-queen) (cdr other-queen)))
		    (not (= (abs (- (car kth-queen) (car other-queen))) (abs (- (cdr kth-queen) (cdr other-queen)))))))
	     (cdr positions)))

(queens 8)
