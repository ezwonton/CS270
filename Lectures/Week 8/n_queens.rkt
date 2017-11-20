#lang racket

;Mark Boady
;CS 270 - Racket Version of N-Queens Problem

;Create a SAT Problem for N-Queens.

;The whole Problem can be simplified into
;At Most One Problems and At Least One Problems

;Given a List of Variables, Select Exactly 1
;1 2 3 4 0
(define (at_least_one L)
  (if (null? L)
      (display "0\n")
      (begin
        (display (first L))
        (display " ")
        (at_least_one (rest L))
        )
   )          
)
;A -> not X and not Y and ...
;not A or (not X and not Y and ...) - Convert implies to not/or
;(not X or not A) and (not a or not y) and (not a or ...) ...
;-1 -2 0
;-1 -3 0
;-1 -4 0
(define (implies_rest key L)
  (if (null? L)
      (display "")
      (begin
        (display "-")
        (display key)
        (display " -")
        (display (first L))
        (display " 0\n")
        (implies_rest key (rest L))
      )
  )
)
;We need to imply starting at any position
(define (at_most_one L)
  (if (null? L)
      (display "");Nothing to do
      (begin
        (implies_rest (first L) (rest L))
        (at_most_one (rest L)) ;Move this to the used section
      )
  )
)
(define (exactly_one L)
  (begin
    (at_least_one L)
    (at_most_one L)
  )
)

;Predict where in the table values fall
;Example 4x4 board
; 1 | 2 | 3 | 4
; -------------
; 5 | 6 | 7 | 8
;--------------
; 9 | 10| 11| 12
;--------------
; 13| 14| 15| 16

;Call the first row/column the 0th row/column
;A row goes from 4*i+1 .. 4*(i+1) by 1
;A column goes from (i+1) .. 4*(4-1)+i+1 by 4
;Diagonals Top+Left to Bottom/Right
;(13), (9 14), (5 10 15), (1 6 11 16), (2 7 12), (3 8), (4)
;Top half
;1 to n*n by 5
;2 to n*n-4 by 5
;3 to n*n-8 by 5
;4 to n*n-12 by 5
;Bottom Half
;5 to n*n-1 by 5 ;i=2 (+ (* n (- i 1)) 1) ... (- (* n n) (- i 1)) 
;9 to n*n-2 by 5
;13 to n*n-3 by 5
;Diagonals Top+Right to Bottom+Left
;(1) (2 5) (3 6 9) (4 7 10 13) (8 11 14) (12 15) (16)
;n to n*n-3 by 3 = (4 7 10 13)
;n*2 to n*n-2 by 3 = (8 11 14)
;n*3 to n*n-1 by 3 = (12 15)
;
;n-(i-1) to (n-i)*(n-1)+(n-(i-1)) by (n-1) = (3 6 9)
;n-i+1 to n^2+1-i*n by n-1
(define (seq start stop step)
  (if (> start stop)
      '()
      (cons start (seq (+ start step) stop step))
  )
)
;i is the index and n is the size of the table
(define (row i n)
  (seq (+ (* n i) 1) (* n (+ i 1)) 1)
)
(define (col i n)
  (seq (+ i 1) (+ (* n (- n 1)) (+ i 1)) n)
)

;At Exactly 1 Queen Per row
;n is the size of the table 4x4 = nxn
(define (exactly_one_rows n)
  (exactly_one_rows_h 0 n)
)
(define (exactly_one_rows_h i n)
  (if (< i n)
      (begin
        (exactly_one (row i n))
        (exactly_one_rows_h (+ i 1) n)
      )
      (display "")
  )
)

;Exactly 1 Queen Per Col
(define (exactly_one_cols n)
  (exactly_one_cols_h 0 n)
)
(define (exactly_one_cols_h i n)
  (if (< i n)
      (begin
        (exactly_one (col i n))
        (exactly_one_cols_h (+ i 1) n)
      )
      (display "")
  )
)

;At Most One TL-BR right half
(define (at_most_one_tlbr1 n)
  (at_most_one_tlbr1_h 1 n)
)
(define (at_most_one_tlbr1_h i n)
  (if (< i n)
      (begin
        (at_most_one (seq i (- (* n n) (* n (- i 1))) (+ n 1)))
        (at_most_one_tlbr1_h (+ i 1) n)
      )
      (display "")
  )
)
;At Most One TL-BR Left Half
(define (at_most_one_tlbr2 n)
  (at_most_one_tlbr2_h 2 n);1 would overlap
)
(define (at_most_one_tlbr2_h i n)
  (if (< i n)
      (begin
        (at_most_one (seq (+ (* n (- i 1)) 1) (- (* n n) (- i 1)) (+ n 1)))
        (at_most_one_tlbr2_h (+ i 1) n)
      )
      (display "")
  )
)
;At Most One TR to BL
(define (at_most_one_trbl1 n)
  (at_most_one_trbl1_h 1 n)
)
(define (at_most_one_trbl1_h i n)
  (if (< i n)
      (begin
        (at_most_one (seq (* n i) (- (* n n) (- n i)) (- n 1)))
        (at_most_one_trbl1_h (+ i 1) n)
      )
      (display "")
  )
)
;n-i+1 to n^2+1-i*n by n-1
(define (at_most_one_trbl2 n)
  (at_most_one_trbl2_h 2 n)
)
(define (at_most_one_trbl2_h i n)
  (if (< i n)
      (begin
        (at_most_one (seq (+ (- n i) 1) (- (+ (* n n) 1) (* i n)) (- n 1)))
        (at_most_one_trbl2_h (+ i 1) n)
      )
      (display "")
  )
)
;Number of Variables
;For an NxN table
;(N)^2
;Number of Rows
;For an NxN table
;N rows for at least one
;sum(n-i,i=1..n)= n^2/2-n/2 for each row
;n*(sum(...))
;N cols for at least one
;sum(n-i,i=1..n)= n^2/2-n/2
;First Set of Diagnonals
;The Middle N-1 + N-2 + ... 1 = (- (/ (* n n) 2) (/ n 2))
;On Either Side N-2+N-3+...1 = Above but starting one smaller
;On Either Size N-3+N-4+...1
(define (closed_sum n)
  (- (/ (* n n) 2) (/ n 2))
)
(define (count_diagonal n)
 (if (< n 1)
     0
     (+ (closed_sum n) (count_diagonal (- n 1)))
 )
)

(define (title_row n)
  (begin
    (display "p cnf ")
    (display (* n n))
    (display " ")
    (display (+
              n ;Row At Least 1
              n ;Col At Least 1
              (* n (closed_sum n)) ;Row Exactly One
              (* n (closed_sum n)) ;Col Exactly One
              (* 2 (+
              (count_diagonal n) ;top right diagonal
              (count_diagonal (- n 1)); bottom left diagonal
              ));There are two diagonals
              ))
    (display "\n")
  )
)



;Make the Actual Boolean Expression
(define (nqueens n)
  (begin
    (title_row n)
    (exactly_one_rows n)
    (exactly_one_cols n)
    (at_most_one_tlbr1 n)
    (at_most_one_tlbr2 n)
    (at_most_one_trbl1 n)
    (at_most_one_trbl2 n)
   )
)

(nqueens 4)