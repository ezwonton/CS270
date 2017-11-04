#lang racket

;Mark Boady and Jeremy Johnson
;CS 270 Drexel University 2017
(define zero 'z)
(define (succ x) (list 's x))

(define (zero? n) (eq? n zero))
(define one (succ zero))
(define two (succ one))
(define three (succ two))
(display "Successor Numbers\n")
(display 'zero)
(display "\n")
(display one)
(display "\n")
(display three)

;Check if something is a number
(define (nat? x)
  (cond
    [(zero? x) #t]
    [(pair? x) (and (eq? (first x) 's) (nat? (second x)))]
    [else #f]
  )
)
(display "\nCheck if something is a number\n")
(nat? three)
(nat? two)
(nat? one)
(nat? zero)
(nat? 12)

;Simple Math
;pred subtract's one
;no negative numbers
(define (pred x) (if (zero? x) zero (second x)))
(display "Test Pred (-1)\n")
(pred three)
(pred one)
(pred zero)

(display "Addition\n")
(define (plus m n)
  (if (zero? m)
      n
      (succ (plus (pred m) n))
  )
)
(plus three two)

(display "Subtraction\n")
(define (sub m n)
  (if (zero? n)
      m
      (pred (sub m (pred n)))
  )
)
(sub three two)

(display "Multiplication\n")
(define (mult m n)
  (if (zero? m)
      zero
      (plus n (mult (pred m) n))
  )
)
(mult three three)

(display "Power\n")
(define (pow m n)
  (if (zero? n)
      one
      (mult m (pow m (pred n)))
  )
)
(pow two three)

(display "Less\n")
;m < n
(define (lt? m n)
  (cond
    [(zero? n) #f]
    [(zero? m) #t]
    [else (lt? (pred m) (pred n))]
  )
)
(display "Test m < n\n")
(lt? zero zero)
(lt? one zero)
(lt? zero one)
(lt? three two)
(lt? one three)

(display "From integers for testing\n")
(define (int_to_succ n)
  (if (<= n 0) zero (succ (int_to_succ (- n 1))))
)
(int_to_succ 8)
(define (succ_to_int n)
  (if (zero? n) 0 (+ 1 (succ_to_int (second n))))
)

;Binary Numbers 101
(display "Binary Numbers\n")
(define (int_to_binary x)
  (if (= 0 x) '()
      (cons (remainder x 2) (int_to_binary (quotient x 2)))
  )
)
(int_to_binary 20)

(define (binary_to_int L)
  (if (null? L) 0
      (+ (first L) (* 2 (binary_to_int (rest L))))
  )
)
(binary_to_int '(0 0 1 0 1))

;Run a Bunch of tests
(define (test_binary n)
  (if (< n 0) #t
      (and (equal? n (binary_to_int (int_to_binary n))) (test_binary (- n 1)))
  )
)
(test_binary 100)

;You will implement the below two problems in lab.
;These are just to give you an idea. They are not full implemented below.

;Binary Addition
;There are 4 bases cases
;0 + 0 = 00
;0 + 1 = 01
;1 + 0 = 01
;1 + 1 = 10 
;The first bit is the carry and the second is the value
;For example,
;1110
; 101 (5)
;+111 (7)
;----
;1100 (12)
;For a single column we have three inputs and two ouputs
;a + b + carry_in = (carry_out, result)
;
; The carry out is passed recursively to the rest of the list
;Basic resursive Idea
;(add carry A B)
;(result_bit (first A) (first B) carry)
;+ (add (carry_out (first A) (first B) carry) (rest A) (rest B))

;Binary Multiplication
;Binary Multiplicaton is handled like regular multiplication.

;   110 (6)
;  *101 (5)
;-------
;   110 (Result of 1 * 110)
;  0000 (Result of 0 * 110 shifted 1 bit to the left)
;+11000 (Result of 1 * 110 shifted 2 bits to the left)
;------
; 11110 (Add up the three values from above)
; Result: 2+4+8+16=30!

;Recursive formula
;A = (a0 ... an)
;B = (b0 ... bm)
;A*B = a0*b + (a1...n)*b*2 (multipling by two just adds a 0)
;(add (mult_bit (first A) b) (mult (rest A) (mult2 b))
