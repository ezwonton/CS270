#lang racket
#|
(display "\nLab Question 1\n")
(define listex '(1 2 3 4 5 6 7 8 9))

(define (sqr x)
  (* x x)
)
(define (map1 f L)
  (cond
    [(null? L) L]
    [(cons (f (first L)) (map1 f (rest L)))]
  )
)
(map sqr listex)
(map1 sqr listex)


(display "\nLab Question 2\n")
(define (reduce f init L)
  (cond
    [(null? L) init]
    [(reduce f (f init (first L)) (rest L))]
  )
)
(foldr + 0 '(1 2 3 4))
(reduce + 0 (list 1 2 3 4))
(+ 1 (reduce + 0 '(2 3 4)))
(+ 1 (+ 2 (+ 3 (+ 4 0) ) ) )


(display "\nLab Question 3\n")
(define (neglist init L)
  (begin
    (display (map1 negative? L))
    (display "\n")
    (display (map1 (lambda (x) (if x 1 0)) (map1 negative? L)))
    (display "\n")
  (cond
    [(null? L) init]
    [(reduce + init (map1 (lambda (x) (if x 1 0)) (map1 negative? L)))]
  ))
)
(define listex2 '(-1 -2 -3 4 -5 6 -7 8 9))
(neglist 0 listex2)
|#
(display "\nLab Question 4\n")
#|
n length dominoes
n-1 with a V
n-2 with 2H
|#

(define (GenDominoes n)
  (if (> n 0)
      (append (map (lambda(x) (string-append "V" x)) (GenDominoes (- n 1)))
              (if (> n 1) (map (lambda(x) (string-append "HH" x)) (GenDominoes (- n 2)))
              '()
              )
      )
      '("")
  )
)
(GenDominoes 0)
(GenDominoes 1)
(GenDominoes 2)
(GenDominoes 3)
(GenDominoes 4)
(GenDominoes 5)
