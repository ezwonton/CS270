#lang racket

#|
CS 270 Math Foundations of CS Midterm Exam (online)
Fall 2017-18
Instructor:  Profs. Jeremy Johnson and Mark Boady

Instructions: The exam will be available starting from Wed. Nov. 1 at 9pm until
Sat. Nov. 4 at 11:59 pm.  Modify this file to include your answers.  Racket code
should be executable and text answers provided in the comment section that includes
the question. If your code is not running provide a comment that indicates this
and if there is a runtime error, comment out your code.  Partial credit will be
given, so you should include your attempt even if it is not working.
When completed the exam must be submitted to BBLearn.  Once started students
will have 2 hours to complete the exam. Students must take the exam individually
and are on a strict honor code. Students may use Dr. Racket, Racket documentation,
course readings and materials. No other materials may be used.  Try to be as concise
as possible with your solutions whether they are code or proofs.  

There are 5 questions each worth 20 points for a total of 100 points.  Some questions
may have multiple parts.  In that case, the number of points for each part is
indicated.

Q1 /20
Q2 /20
Q3 /20
Q4 /20
Q5 /20
Total /100

|#

(require racket/contract)
(require rackunit)
(require rackunit/text-ui)

#|
Question 1.  Truth Tables and Boolean Algebra – 20 Points

Part 1) [10 points]  Use a truth table to show that the following formula
        (absorption) is valid.   

                              A \/ (A /\ B)  <->  A



Part 2) [10 points]   Use laws of Boolean algebra, given in the slides from
        the lecture on Boolean Algebra and Simplification, to prove the
        identity in Part 1.  You may not use the absorption law itself.  You
        must provide all steps in the proof along with laws used in each step.


|#

#|
Question 2.  Racket Semantics and List Processing – 20 Points

Consider the Racket function (unknown L) given below.

Part 1) [5 points]  Provide input and output specifications including a
        clear statement what the unknown function does.

Part 2) [10 points]  Trace through, by hand, the evaluation of the expression
        (unknown ‘(1 2 3)).  You should show each intermediate step.

Part 3) [5 points]  Show how, only using cons, to build the list ‘( () () )

|#

(define (unknown L)
  (cond
    [(null? L) (error "empty list")]
    [(null? (rest L)) (first L)]
    [else (unknown (rest L))]))

#|
Question 3.  Recursion – 20 points

Write a recursive Racket function that computes the union of two sets, where
sets are represented by lists which do not contain any duplicates.  Recall that
the union of two sets is a set that contains all elements from both sets without
duplicates.  You may use the function (member? x S) provide below and the basic
list processing functions cons, null?, first and rest along with Racket special
forms cond and define.  No other Racket functions may be used.

|#

(define (member? x S)
  (cond
    [(null? S) #f]
    [(equal? x (first S)) #t]
    [else (member? x (rest S))]))

#|
Question 4.  Map/Reduce and Higher Order Functions – 20 points

Part 1) [5 points]  Write a function (charfun pred?) which takes as input a
        predicate function pred? and returns as output a function which
        returns 1 if the predicate pred? is #t and 0 otherwise.

Part 2) [10 points] The Racket math/number-theory library contains a predicate
        prime? which returns #t if the input is a prime number and #f otherwise.
        Show how to use map and foldr to count the number of primes in a list of
        integers.  Apply this to (range 1 100) which returns a list of integers
        between 1 and 100 to count the number of primes between 1 and 100.

Part 3) [5 points]  Show how to use foldr to implement the append function.

|#

(require math/number-theory)

#|
Question 5.  Boolean Expressions – 20 points

In Lab 5 we wrote a function that evaluates boolean expressions and you
extended this function to allow the implication of two boolean expressions.
In this question you will extend this further to allow the equivalence of two
boolean expressions, i.e. (iff E1 E2).  However, this time you are not allowed
to modify boolean-eval.

Specifically, replace each ***** with the appropriate code to write a function
called "remove-iff" which takes as input a boolean expression and produces an
equivalent boolean expression with all (iff E1 E2) replaced by
(and (implies E1 E2) (implies E2 E1)).  For your convenience the predicates
constant?, variable?, etc. along with the access functions op1 and op2 are
provided below.  Note that you will need to write the iff? predicate.

(define (remove-iff expr)
    (cond
    [(constant? expr)  ***** ]
    [(variable? expr)  *****]
    [(negation? expr)  *****]
    [(disjunction? expr) *****]
    [(conjunction? expr)  *****]
    [(implication? expr)  *****]
    [(iff? expr)  ***** ]
    ))

|#

; predicate to check for symbols that are reserved.
(define (is-reserved-word? word)
  (cond
   [ (eq? word '#t) #t]
   [ (eq? word '#f) #t]
   [ (eq? word 'true) #t]
   [ (eq? word 'false) #t]
   [ (eq? word 'or) #t]
   [ (eq? word 'and) #t]
   [ (eq? word 'not) #t]
   [ (eq? word 'implies) #t]
   [ (eq? word 'iff) #t]
   [ (eq? word 'xor) #t]
   ;Otherwise
   [ else #f]
  )
 )
; Predicate to check for Boolean constant
(define (constant? expr)
  (or (eq? expr 'true) (eq? expr 'false)))

; Predicate to check for a variable
(define (variable? expr)
  (and (symbol? expr) (not (is-reserved-word? expr))))

; Predicate to check for a negation
(define (negation? expr)
  (and (list? expr)
       (= (length expr) 2)
       (eq? (first expr) 'not)))

; Predicate to check for a disjunction
(define (disjunction? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (first expr) 'or)))

; Predicate to check for a conjunction
(define (conjunction? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (first expr) 'and)))

; Predicate to check for a implication
(define (implication? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (first expr) 'implies)))

; access functions for Boolean expressions
(define (op1 expr)
  (second expr))

(define (op2 expr)
  (third expr))
