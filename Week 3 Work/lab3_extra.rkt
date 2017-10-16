#lang racket

#|
Lab 3:  Bits, bits and more bits
CS 270 (Math Foundations of CS)

In this lab students implement recursive functions to add and multiply two
numbers represented in binary.

Binary numbers are represented by a list of bits.
(b_0 b_1 ... b_{n-1}) represents b = b_0 + b_1*2 + ... + b_{n-1}*2^{n-1}
For example, the binary representation for 13 is 1101 and is represented
by the list (1 0 1 1).

The number zero is represented by the empty list.
Note that representations are not unique.  E.G. 0 = () and also (0 0 0)
We will assume binary numbers are normalized.  I.E. there are no
leading zeros.  Normalized binary numbers are unique.

In part 1 students implement binary addition using the following
recursive construction.

a = (a_0 a_1 ...a_{m-1}) and b = (b_0 b_1 ... b_{n-1})

a + b = a_0 + b_0 + 2*[a' + b'], where a' = (a_1 ...a_{m-1}) and
b' =(b_1 ... b_{n-1}).

The multiplication by 2 after the recursive call corresponds to a shift
and consequently the result of the a'+b' is one position to the right of
the sum of a_0 and b_0.  Thus the sum of a_0 and b_0 may simply be consed
onto the result of the recursive call.

When adding the bits a_0 and b_0 there may be a carry.  I.E.
when both a_0 and b_0 are 1 the result is 2.  Thus the low order bit of
the result is 0 and 1, the carry, must be added to the recursive sum a'+b'.
In general when adding two bits and a carry the maximum result is 3 which
implies the sum bit is either 0 or 1 and the carry is also either 0 or 1.

Thus when implementing binary addition we include an extra input equal to
the carry in, called cin, and compute (binadd cin a b) = cin + a + b.
The above recursive construction becomes

a + b + cin = (a_0 + b_0 + cin) + 2*[a'+b'] which is equal to
c_0 + 2*[cout + a' + b'], where c_0 = (a_0 + b_0 + cin) mod 2 and
cout = 1 when (a_0 + b_0 + cin) > 1 and 0 otherwise.

In part 2 students implement binary multiplication using the following
recursive construction.

a*b = a_0*b + 2*(a_1 ... a_{m-1})*b

Multiplication by a power of two is easy when numbers are in binary.
Multiplicatation by 2^h shifts the number h places to the right which
corresponds to prepending h leading zeros.
2^h*b = (0 ... 0 b_0 b_1 ... b_{n-1})

As an aid to implementing these functions you may wish to use the
let* special form to introduce names for temporary values that are used
later.

The syntax (let* (b1 ... bt) exp)
introduces bindings b1,...,bt which can be used in exp which is the
result that is returned.  let* as compared to let allows preceding
bindings to be used in the computation of the values for the subsequent
bindings.

E.G.  (let* ((x 1) (y (+ x 3))) (+ x y)) returns 5

|#

(require racket/contract)
(require rackunit)
(require rackunit/text-ui)

(define (binzero? a)
  (null? a))

(define zero '())

(define one '(1))

; (binadd cin a b) adds two binary numbers with a potential carry
; Inputs: cin = 0 or 1, a and b are normalized binary numbers
; Output: a normalized binary number = a + b + cin.
;
; Note: cin is needed for recursive calls when a carry, cout, is
; introduced.
;


(define (binadd cin a b)
  (begin
    (display (binzero? a))
    (display (binzero? b))
    (display (zero? cin))
    (display "\n")
    
  (cond
    [(binzero? a) (binadd cin 0 b)]
    [(binzero? b) (binadd cin a 0)]
    [(zero? cin) (binadd 0 a b)]
    [else (cons (remainder (+ cin (rest a) (rest b)) 2))]
  ))
)

; Unit tests - tests binadd.
(define-test-suite binadd-suite

  (check-equal? 
    (binadd 0 '() '()) '())
  
  (check-equal? 
    (binadd 1 '() '()) '(1))
#|
  (check-equal? 
    (binadd 0 '() '(1 0 1)) '(1 0 1))

  (check-equal? 
    (binadd 0 '(1 0 1) '()) '(1 0 1))

  (check-equal? 
    (binadd 1 '() '(1 0 1)) '(0 1 1))

  (check-equal? 
    (binadd 1 '(1 0 1) '()) '(0 1 1))

  (check-equal? 
    (binadd 0 '(1) '(1 1 1)) '(0 0 0 1))

  (check-equal? 
    (binadd 0 '(1 1 1) '(1)) '(0 0 0 1))

  (check-equal? 
    (binadd 0 '(1 0 1) '(0 1 1)) '(1 1 0 1))
  
  (check-equal? 
    (binadd 0 '(1 1 1) '(1 1 1)) '(0 1 1 1))
|#
)
(print "Running binadd tests")  (newline)
(run-tests binadd-suite 'verbose)

; (binmult2 h b) multiplies a binary number by a power of two
; Inputs:  h a non-negative integer and b a binary number.
; Output: a binary number equal to 2^h*b
;
; Note: multiplication of a binary number by a power of two is
; just a shift, where h leading zeros are inserted before the trailing
; bit of the binary number.
#|
(define (binmult2 h b)
 "not implemented yet")

; (binmult h a b) computes the product of two binary numbers multiplied
; by a power of two.
; Inputs: h a non-negative integer, a and b normalized binary numbers.
; Output: a normalized binary number equal to 2^h*a*b.
;
; Note: Let a = (a_0 a_1 ... a_{m-1}).  The following recursive construction
; is used, which shows why the parameter h is included.
; a*b = a_0*b + 2*(a_1 ... a_{m-1})*b

(define (binmult a b)
  "not implemented yet")

; Unit tests - tests binmult.
(define-test-suite binmult-suite
  
  (check-equal? 
    (binmult2 0 '(1 0 1)) '(1 0 1))

  (check-equal? 
    (binmult2 3 '(1 0 1)) '(0 0 0 1 0 1))

  (check-equal? 
    (binmult zero '(1 0 1)) zero)

  (check-equal? 
    (binmult '(1 0 1) zero) zero)

  (check-equal? 
    (binmult one '(1 0 1)) '(1 0 1))

  (check-equal? 
    (binmult '(1 0 1) one) '(1 0 1))
  
  (check-equal? 
    (binmult '(0 0 1) '(1 0 1)) '(0 0 1 0 1))

  (check-equal? 
    (binmult '(1 0 1) '(0 0 1)) '(0 0 1 0 1))

  (check-equal? 
    (binmult '(1 0 1) '(1 0 1)) '(1 0 0 1 1))
)
(print "Running binmult tests")  (newline)
(run-tests binmult-suite 'verbose)
|#