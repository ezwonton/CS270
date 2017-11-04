#lang racket
(define zero 'z)
(define (zero? x) (if (eq? x 'z) #t #f))
(define (succ x) (list 's x));s means +1
(define (pred x) (if (zero? x) x (second x)))
(define one (succ zero))
(define two (succ one))
(define three (succ two))

(define (nat? x)
  (cond
    [(zero? x) #t]
    [(pair? x)
     (and (eq? (first x) 's) (nat? (second x)))]
    [else #f]))
(nat? three)
'(s (s (s z)))
(define (plus a b)
  (cond
  [(zero? a) b]
  [else (succ (plus (pred a) b))]))
(plus three three)
(define (mult a b)
  (cond
    [(zero? a) zero]
    [else (plus b (mult (pred a) b))]))
(mult three three)












