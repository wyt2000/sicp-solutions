#lang sicp

(define (f g)
  (g 2))

(f f)

;application: not a procedure;
; expected a procedure that can be applied to arguments
;  given: 2
;  context...:
;   /algo/sicp-solutions/chapter1/1-3/1-34.rkt:6:0
;   body of "/algo/sicp-solutions/chapter1/1-3/1-34.rkt"
;
;(f f) = (f 2), but 2 is not a function.
