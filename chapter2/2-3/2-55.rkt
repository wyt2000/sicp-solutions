#lang sicp

(car ''abracadabra)
;'xxx will be interpreted as (quote xxx)
;so (car ''xxx) -> (car '(quote xxx)) -> (car (list 'quote 'xxx)) -> quote
