#lang racket

;Mark Boady
;CS 270 - Drexel University - 2017
;Week 3 (Pile of Rocks) Lecture

;Number Functions built into Racket
;The number predicate works with any type of number
;Racket even supports fractions.
(display "Number Predicate\n")
(number? 12)
(number? (/ 1 2))
(number? 1.34)
(number? '(1 2 3))

;Basic Arithmetic
(display "Basic Arithmetic\n")
(+ 1 2)
(- 3 4)
(* 4 5)
(/ 1 4) ;Notice this makes a fraction
(quotient 5 4);Integer Division Result
(remainder 5 4);Mod symbol in many languages

;Comparison Operators for numbers
(display "Comparison Operators\n")
(= 1 2)
(< 1 2)
(> 5 7)
(<= 7 7)
(>= 9 28)

;Other Advanced Functions
(display "Advanced Functions\n")
(abs -9) ; Get Absolute Value
(round 1.25); round to nearest integer
(round (/ 1 4))
(gcd 27 9);Find the greatest common demoninator
(sqrt 27);Square Root returns a decimal
(sqrt 4);if not exact
(exp 9);e^9 the exponential base e
(expt 2 9);2^9 use the base of your choice
(log (exp 9));Default log base is e. This is also called ln.
(cos 9)
(sin 9)
(sin pi)


(display "Pile of Rocks\n")
;First Representation of Number Recursively
;We have a list of 1s.
;This is a pile of rocks counting method.
(define (zero? n) (null? n));Zero is no rocks
(define zero null)
(define one '(rock))
(define two '(rock rock))
(define three '(rock rock rock))

;We will not support negative numbers in this example
;Basic addition and subtraction
(define (add1 n) (cons 'rock n))
(define (sub1 n) (if (zero? n) zero (rest n)));No Negatives

;A natural number is a zero or a list of rocks
(define (nat? n)
  (cond
    [(zero? n) #t]
    [(cons? n) (and (eq? (first n) 'rock) (nat? (sub1 n)))]
    [else #f]
  )
)
;We use sub1 instead of rest to make it more consistently math-y

;Recursive Defintion of Add
;m+n = 1 + (m-1) +n if m>0
;0+n = n
(define (add m n)
  (if (zero? m)
      n
      (add1 (add (sub1 m) n))
  )
)
(display "Testing Add\n")
(add three two)

;Multiply defined in terms of add
;0*n=0
;m*n=n+(m-1)*n
(define (mult m n)
  (if (zero? m)
      zero
      (add n (mult (sub1 m) n))
  )
)
(display "Testing Multiply\n")
(mult three two)

;Equal
(define (eq_rock? m n)
  (cond
    [(zero? m) (zero? n)];0=0
    [(zero? n) #f];0!=nonzero
    [else (eq_rock? (sub1 m) (sub1 n))];Count them both
  )
)
(display "Testing Equals\n")
(eq_rock? zero zero)
(eq_rock? one zero)
(eq_rock? zero one)
(eq_rock? three three)
(eq_rock? three two)

;a>b
(define (gt? m n)
  (cond
    [(zero? m) #f]
    [(zero? n) #t]
    [else (gt? (sub1 m) (sub1 n))]
  )
)
(display "Testing Greater\n")
(gt? zero two)
(gt? two zero)
(gt? zero zero)
(gt? three two)
(gt? two three)