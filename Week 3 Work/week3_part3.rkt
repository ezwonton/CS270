#lang racket
; 101 4*1 + 2*0 + 1*1
;'(1 0 1)
;'(0 0 1) 0*1 + 0*2 + 1*4 = 4
(define (int_to_binary n)
  (cond
    [(< n 0) (error "Negative Number!")]
    [(= n 0) '()]
    [else (cons (remainder n 2)
          (int_to_binary (quotient n 2)))]))
(int_to_binary 6)
;6/2 = 3R0
;3/2 = 1R1
;1/2 = 0R1
;(0 1 1)
(define (binary_to_int n)
  (cond
    [(null? n) 0]
    [(eq? (first n) 1)
     (+ 1 (* 2 (binary_to_int (rest n))))]
    [(eq? (first n) 0)
     (* 2 (binary_to_int (rest n)))]))
(binary_to_int '(1 0 1))


(define zero '())
(define (double x) (cons 0 x))
(define (doublep1 x) (cons 1 x))



  






