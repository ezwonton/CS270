#lang racket

;Question 3

(define (parity a b cin)
  (odd? (+ a b cin) 1 0)
)

(define (parityBool list)
  (cond
    [(null? list) #f]
    [(eq? list '(0)) #f]
    [(eq? list '(1)) #t]
    [else (xor (if (eq? (first list) 1) #t #f) (parityBool (rest list)))]
  )
)
;(parityBool '(1 1 0 1)) -> 1
;(parityBool '(0 1 0 1)) -> 0
(parityBool '(1 1 0 1))
(parityBool '(1 1 0 1 0 0))
(parityBool '(0 1 0 1))

(define (majorityBool list)
  (cond
    [(null? list) #f]
    [(eq? list '(0)) #f]
    [(eq? list '(1)) #t]
    [else (or (and (if (eq? (first list) 1) #t #f) (if (eq? (second list) 1) #t #f))
              (and (if (eq? (third list) 1) #t #f) (parityBool (rest list))))
              ]
  )
)
(majorityBool '(1 1 0 1))
(majorityBool '(0 0 0 1))
(majorityBool '(1 1 0 1 0 0))