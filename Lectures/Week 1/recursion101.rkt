#lang racket
;CS 270 - Week 1 Lecture Notes
;Mark Boady
;Drexel University
;Spring 2017

;Basic Racket Programming
;Every Function Call has the form (function_name input_1 input_2 ect)
;Examples:
; 7 < 8 is
(< 7 8)
; 10 + 5 is
(+ 10 5)
; if(5 < 7){return 5;}{return 9;} is
(if (< 5 7) 5 9)
;No Loops!
;No "Variables"
;Printing is done using (display "Some Text")
;You can execute a series of commands using begin
(begin
  (display "Command 1\n")
  (display "Command 2\n")
  (display "Command 3\n")
  (+ 8 7)
)
;True and False are #t and #f

;Defining your own function
;(define (function_name input1 input2 ...) (function body) )
(define (sqr a) (* a a))
(sqr 2)
(sqr 3)
(sqr 4)

;How do we use recursion to replace loops?
;One easy way is to use two functions.

#|
Here is a c++ example
int sum(int n)
{
	int total=0;
	int i=0;
	while(i <= n)
	{
		total=total+i;
		i++;
	}
	return total;
}
|#
;This functions set the initial values
;i=0 and total=0
(define (sum n)
  (sum_helper n 0 0)
)
;This function implements the actual loop.
;The base case is the opposite of the loop condition
;while(i <= n) becomes (if (not (<= i n)) ...)
;The return value is the base case
;return total becomes (if (...) total (...))
;The loop body becomes the recursive case
;total=total+i and i++ becomes (sum_helper n (+ i 1) (+ total i))

;Definitions:
;Recursion: A function that calls itself directly or indirectly.
;Base Case: The part of a recursive function that is not defined recursively. The stopping condition.
;Recursive Case: The part of a recursive function that includes a recursive call.
(define (sum_helper n i total)
  (if (not (<= i n))
      total
      (sum_helper n (+ i 1) (+ total i))
  )
)

;This function replicates a for loop
;for(int x=start; x <= stop; x+=inc)
(define (test_sum start stop inc)
  (if (> start stop)
      '();This is the equivelant of "do nothing" return a null list
      (begin
        (display "sum(")
        (display start)
        (display ")=")
        (display (sum start))
        (display "\n")
        (test_sum (+ start inc) stop inc)
       )
  )
)
;Inputs are all set when calling the function
(test_sum 0 10 1)

;This function has a closed form we can implement instead!
;Note: sum(i,i=0..n) = 1/2n^2+1/2n
(define (quick_sum n)
(+ (* (/ 1 2) (* n n)) (* (/ 1 2) n))
)

(quick_sum 10)