#lang racket
(define (zero? x) (null? x))
(define zero '())
(define one '(1))
(define two '(1 1))
(define three '(1 1 1))
;number: null list | (cons 1 number)
(define (add1 n) (cons 1 n))
(define four (add1 three))
(display four)(display "\n")
(define (sub1 n) (if (zero? n) n (rest n)))
(sub1 three)


(define (nat? n) (cond
    [(zero? n) #t]
    [(cons? n)
     (and (eq? (first n) 1) (nat? (sub1 n)))]
    [else #f]))
(nat? 7) (nat? '()) (nat? '(1))
(nat? '(1 1 a))

(define (add m n)
  (if (zero? m)
      n
      (add1 (add (sub1 m) n))))
(add three two)
(define (sub m n)
  (cond
    [(zero? m) (error "Negative Number!")]
    [(zero? n) m]
    [else (sub1 (sub m (sub1 n)))]))
(sub three two)
(sub three three)
(define (mult m n)
  (cond
    [(zero? m) zero]
    ;[(zero? (sub1 m)) n]
    [else (add n (mult (sub1 m) n))]))
(mult three three)

(define (pow a b)
  (cond
    [(zero? b) (add1 zero)]
    [else (mult a (pow a (sub1 b)))]))
(length (pow three three))

(define (gt? a b) ;a > b
  (cond
    [(zero? a) #f]; 0 > a false
    [(zero? b) #t]; b > 0 true
    [else (gt? (sub1 a) (sub1 b))]
  ))
(gt? three two)



















