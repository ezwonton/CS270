#lang racket
(define binzero 'Z)
(define (double n) (list 'D n))
(define (double-plus1 n) (list 'DP1 n))
(define (zero? x) (equal? x 'Z))
(define (double? x)
  (and (not (null? x)) (equal? (length x) 2)
       (equal? (first x) 'D)))
(define (double-plus1? x)
  (and (not (null? x)) (equal? (length x) 2)
       (equal? (first x) 'DP1)))
(define (bin_to_int x)
  (cond
    [(zero? x) 0]
    [(double? x) (* 2 (bin_to_int (second x)))]
    [(double-plus1? x)
     (+ 1 (* 2 (bin_to_int (second x))))]))

(define y '(D (DP1 (D Z))))
(bin_to_int y)

(define (int_to_bin x)
  (cond [(= x 0) 'Z]
    [(= (remainder x 2) 1)
     (double-plus1 (int_to_bin (quotient x 2)))]
    [(= (remainder x 2) 0)
     (double (int_to_bin (quotient x 2)))]))
(bin_to_int (int_to_bin 255))




