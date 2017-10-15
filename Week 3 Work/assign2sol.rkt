#lang racket

#|
CS 270 Math Foundations of CS
Fall 2017-18
Instructor:  Profs. Jeremy Johnson and Mark Boady
Due Thur. Oct. 26 @ 9am
Submit in BBLearn and include a readme.txt file which
briefly summarizes any difficulties you encountered and
any problems you could not solve.  Also provide an estimate
of how long it took you to complete the assignment.

Assignment 2
Recursive definitions and functions

In this assignment students will write several functions that
recursively process numbers.  In class we provided two recursive
definitions of numbers.  The recursive definitions provide a set
of constructors which capture the ways to construct a number.
Peano numbers have two constructors, one of which, succ, is recursive,
and Binary Numbers have three constructors, two of which, double and
double-plus1 are recursive.

1)  Peano arithmetic

    In words:  A number is either zero, or, recursively, the successor
    of a number is a number.

    Formally:  Number := zero|(succ Number)

2)  Binary arithmetic

    In words:  A binary number is either zero, or recursively, doubling
    a number or doubling a number and adding one gives a binary number.

    Formally:  BinNumber := zero|(double BinNumber)|(double-plus1 BinNumber)

    We can interpret binary numbers by assigning zero to the number zero
    and if b is the value of a binary number than (double b) has value 2*b
    and (double-plus1 b) has value 2*b+1.

    Note that there is more than one way to construct binary numbers with
    the same value.  E.G.  (double zero) has the same value as zero.
    A binary number is normalized if (double zero) is recursively replaced
    by zero, i.e. trailing zeros are removed.

    When recursively processing either types of numbers, you must
    handle cases corresponding to the different constructors.  Recursive
    calls must have inputs whose size is smaller, where size is the
    number of constructors needed to build the number.

    In the first part of this assignment you will write recursive
    functions to subtract and divide Peano numbers.  You will also
    implement the Euclidean algorithm to compute the greatest common
    divisor, gcd, of Peano numbers.

    In the second part of this assignment you will write recursive functions
    to add and multiply Binary numbers using the recursive definition above.

|#

(require racket/contract)
(require rackunit)
(require rackunit/text-ui)

#|

Part I.  Peano arithmetic.

Question 1.  Implement subtraction of Peano numbers.  See specification
             below.

Question 2.  Implement division of Peano numbers.  Implement functions to
             compute the quotient and remainder when dividing Peano numbers
             m and n.  I.E. compute q the quotient and r the remainder
             such that m = q*n + r with 0 <= r < n.  See specification
             below.

Question 3.  Implement a function to compute the greatest common divisor
             of the Peano numbers m and n.  g = gcd(m,n) satisfies
             1)  g is a common divisor of m and n.
                 g divides m and g divides n.  I.E. the remainder when
                 dividing m and n by g is 0.
             2)  g is the greatest common divisor.
                 If e divides m and e divides n then e must divide g.

             The gcd(m,n) can be computed recursively.
             1)  gcd(m,0) = m
             2)  gcd(m,n) = gcd(n,remainder of m divided by n).

For questions 1-3 you should uncomment the appropriate unit tests.
|#

(define (zero? n)
  (eq? n 'zero))

(define (nat? x)
  (cond
    [(zero? x) #t]
    [(pair? x) (and (eq? (first x) 'succ) (nat? (second x)))]
    [else #f]))

(define (succ n)
  (list 'succ n))

(define (pred n)
  (if (zero? n) 'zero (second n)))

(define zero 'zero)
(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))
(define six (succ five))
(define seven (succ six))
(define eight (succ seven))
(define nine (succ eight))
(define ten (succ nine))

; addition of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value is m+n
(define (plus m n)
  (if (zero? m)
      n
      (succ (plus (pred m) n))))

; multiplication of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value is m*n
(define (mult m n)
  (if (eq? m 'zero)
      'zero
      (plus n (mult (pred m) n))))
; comparison of Peano numbers
; Input: m, n Peano numbers
; Output: a boolean = #t if the value of m < value of n and #f otherwise
(define (ltnat? m n)
  (cond
    [(zero? n) #f]
    [(zero? m) #t]
    [else (ltnat? (pred m) (pred n))]))

; subtraction of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value is m-n if m >= n.
;         It is undefined otherwise.
(define (sub m n)
  "sub not implemented yet")

; Division of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value q is the quotient of m divided by n.
;         m = q*n + r with 0 <= r < n.
(define (div m n)
  "div not implemented yet")

; Remainder of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value r is the remainder of m divided by n.
;         m = q*n + r with 0 <= r < n.
(define (rem m n)
  "rem not implemented yet")

; Greatest common divisor of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number equal to gcd(m,n).
; Note:  See definition of gcd and algorithm for computing the gcd above.
(define (gcd m n)
   "gcd not implemented yet")
  
#|  
; Unit tests - tests Peano arithmetic.
(define-test-suite peano-suite

  (check-equal? 
    (sub three zero) three)

  (check-equal? 
    (sub three one) two)

  (check-equal? 
    (div ten three) three)

  (check-equal? 
    (rem ten three) one)

  (check-equal?
     (gcd five zero) five)

  (check-equal?
     (gcd (mult four six) (mult three five)) three)

  )
  
(print "Running Peano number tests")  (newline)
(run-tests peano-suite 'verbose)
|#
  
#|

Part II.  Binary arithmetic.

Question 4.  Implement a recursive function to add two binary numbers.
             See the specification below.  Uncomment the corresponding
             unit test to test your implementation.

Question 5.  Implement a recursive function to multiply two binary numbers.
             See the specification below.  Uncomment the corresponding
             unit test to test your implementation.

|#

(define binzero 'zero)

(define (binzero? b)
  (eq? b binzero))

(define (double b)
  (list 'D b))

(define (double-plus1 b)
  (list 'DP1 b))

(define (double? b)
  (cond
    [(not (pair? b)) #f]
    [(eq? (first b) 'D) #t]
    [else #f]))

(define (double-plus1? b)
  (cond
    [(not (pair? b)) #f]
    [(eq? (first b) 'DP1) #t]
    [else #f]))

(define (op b)
  (second b))

(define (binone? b)
  (equal? b binone))

(define binone (double-plus1 binzero))
(define bintwo (double binone))
(define binthree (double-plus1 binone))

; increment a binary number
; Inputs: a binary number b
; Output: a binary whose value is the value of b + 1.
;         if b is normalized (inc b) will be normalized.
(define(inc b)
  (cond
    [(binzero? b) (double-plus1 b)]
    [(double? b) (double-plus1 (op b))]
    [(double-plus1? b) (double (inc (op b)))]))

; add two binary numbers
; Inputs: a and b binary numbers.
; Output: a binary whose value is the value of a + b.
;         if a and b are normalized (plus a b) will be normalized.
(define (binplus a b)
  "binplus not implemented yet")
#|
; Unit tests - tests binplus.
(define-test-suite binplus-suite

  (check-equal? 
    (binplus binzero binthree) binthree)

  (check-equal? 
    (binplus binthree binzero) binthree)
  
  (check-equal? 
    (binplus bintwo binone) binthree)

  (check-equal? 
    (binplus bintwo bintwo) '(D (D (DP1 zero))))

  (check-equal? 
    (binplus bintwo binthree) '(DP1 (D (DP1 zero))))

  (check-equal? 
    (binplus binthree binthree) '(D (DP1 (DP1 zero))))
  )
  
(print "Running binplus tests")  (newline)
(run-tests binplus-suite 'verbose)
|#
  
; multiply two binary numbers
; Inputs: a and b binary numbers.
; Output: a binary whose value is the value of a * b.
;         if a and b are normalized (mult a b) will be normalized.
; Note:  in order to guarantee that the result is normalized, you
;        must handle the case when a or b is one separately.  To
;        see why, compute (mult one one)

(define (binmult a b)
   "binmult not implemented yet")
#|  
; Unit tests - tests binmult.
(define-test-suite binmult-suite

  (check-equal? 
    (binmult binzero binthree) binzero)

  (check-equal? 
    (binmult binthree binzero) binzero)

    (check-equal? 
    (binmult binone binthree) binthree)

  (check-equal? 
    (binmult binthree binone) binthree)
  
  (check-equal? 
    (binmult bintwo bintwo) '(D (D (DP1 zero))))

  (check-equal? 
    (binmult bintwo binthree) '(D (DP1 (DP1 zero))))

  (check-equal? 
    (binmult binthree bintwo) '(D (DP1 (DP1 zero))))

  (check-equal? 
    (binmult binthree binthree) '(DP1 (D (D (DP1 zero)))))
  )
  
(print "Running binmult tests")  (newline)
(run-tests binmult-suite 'verbose)
|#