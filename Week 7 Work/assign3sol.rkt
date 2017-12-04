#lang racket
#|
CS 270 - Mathematical Foundations of Computer Science
Drexel University Fall 2017-2018
Homework Assignment 3
Due Thur Nov. 16 at 9:00am

Submit in BBLearn and include a readme.txt file which
briefly summarizes any difficulties you encountered and
any problems you could not solve.  Also provide an estimate
of how long it took you to complete the assignment.


Student name:  Solution by Jeremy Johnson

The goal of this assignment is to further develop recursive programming
skills and to reason informally about recursive programs in addition to
providing informal and formal (using Racket itself) specifications.
In a followup assignment students will reason formally about these
recursive functions.  The assignment has four parts:  Part 0 provides
useful helper functions, Part I provides functions to convert to negative
normal form, Part II asks you to provide functions to convert to conjunctive
normal form and Part III asks you to write a function to check that two
boolean expressions are equivalent.  Parts 0 and I are provided to you and
you only need to study the code in these parts.  Parts II and III require
that you write and test functions for which you may use the functions in Parts
0 and I.


Instructions for using this file:

- open this file in DrRacket as assign3.rkt

- insert your solutions into this file where indicated (for instance as
"'replace-this-with-your-implementation")

- make sure the entire file is accepted by DrRacket. If you don't finish some
problems, comment them out. The same is true for any English text that you may add.
This file already contains many comments, so you can see what the syntax is.

- All function names are given, do not change the names of any functions

|#


;; We use rackunit package to do unit tests. When you start,
;; all the tests will be failing. Once you implement the required
;; functions, the unit tests associated with those functions should
;; pass.
;; 
;;

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment Overview
;
; The goal of this assignment is to create a function that converts
; an arbitrary boolean expression into an equivalent one in conjuctive
; normal form (CNF).  Recall that CNF is a conjunction (and) of
; clauses which are disjunctions (or) of literals.  A literal is either
; a variable or a negated variable.  Note that a single clause is in
; CNF as is a single literal or constant.
;
; For this assignment we assume that boolean expression only consist of
; constants, variables, not, and, and or.  Other boolean functions such
; as implies, and iff can be converted into equivalent expressions using
; just constants, variables, not, and, and or. Using the tautologies
; (implies a b) <-> (or (not a) b) and
; (iff a b) <-> (and (implies a b) (implies b a))
;
; Part 0 of the assignment provides boolean-eval and other helper functions
; such as op1, op2, negation?, disjunction?, and conjunction? that you can use in the
; rest of the assignment.  boolean-eval provides a template for writing
; recursive functions for processing boolean expressions.
;
; Conversion to conjunctive normal form is done in two steps:
; 1) [nnf] convert to negative normal form using DeMorgan's law
; 2) [cnf] convert expressions in negative normal form to conjunctive normal form
;    using the distributive law.
;
; Separate functions nnf? and cnf? are written to check that the output
; from nnf and cnf is in negative normal form or conjunctive normal form respectively.
; Code for (1) is provided in Part I.  Students should study this code before
; writing code for (2) in Part II.  In Part II students write cnf? and
; the function cnf along with helper functions that guide them through
; the implementation of cnf? and cnf.  In Part III students write a
; function is-equivalent? to test of two boolean expressions are equivalent.
; For the function cnf to be correct the output must be in CNF and it must be
; equivalent to the input. Test suites are provided to test and help specify cnf?,
; cnf and is-equivalent?.  The test suites are commented out.
; 
; When you are testing these functions you should uncomment
; the test suites.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 0.  Auxiliary and helper functions that may be used in the assignment
;
;          bool-eval, lookup,op1, op2,
;          is-reserved-word? constant?, variable?, negation?, disjunction?
;          conjunction?, implication?, boolean-eval, is-tautology?


; Function to evaluate a Boolean expression
(define (boolean-eval expr env)
  (cond
    [(constant? expr) (if (eq? expr 'true) #t #f)]
    [(variable? expr) (lookup expr env)]
    [(negation? expr) (not (boolean-eval (op1 expr) env))]
    [(disjunction? expr) (or (boolean-eval (op1 expr) env)
                              (boolean-eval (op2 expr) env))]
    [(conjunction? expr) (and (boolean-eval (op1 expr) env)
                              (boolean-eval (op2 expr) env))]
    [(implication? expr) (implies (boolean-eval (op1 expr) env)
                              (boolean-eval (op2 expr) env))]
    ))

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

; construct binding
(define (binding var value)
  (list var value))

; (get-var-in-binding b)
; Inputs: b = (var val) a binding
; Output: var the variable in b

(define (get-var-in-binding b)
  (first b))

; (get-val-in-binding b)
; Inputs: b = (var val) a binding
; Output: val the value in b

(define(get-val-in-binding b)
  (second b))
; (lookup? var env)
; Inputs: var is a variable - Racket symbol and env is an environment.
; Output: if var is in env, then the S-expression associated with var
;         if var is not in env an error is thrown using the built-in
;         error function.
(define (lookup var env)
  (cond
    [(null? env) (error var "not found")]
    [(eq? var (get-var-in-binding (first env)))
     (get-val-in-binding (first env))]
    [else (lookup var (rest env))]))

(define (union S1 S2)
  (remove-duplicates (append S1 S2)))

; Function to get variables in a Boolean expression
(define (get-variables expr)
  (cond
    [(constant? expr) null]
    [(variable? expr) (list expr)]
    [(negation? expr) (get-variables (op1 expr))]
    [(disjunction? expr)(union (get-variables (op1 expr))
                              (get-variables (op2 expr)))]
    [(conjunction? expr) (union (get-variables (op1 expr))
                              (get-variables (op2 expr)))]
    [(implication? expr) (union (get-variables (op1 expr))
                              (get-variables (op2 expr)))]
    ))

(define (generate-truth-table vars)
  (if (null? vars)
      '(())
      (append
       (map (lambda (env) (cons (binding (first vars) #t) env))
            (generate-truth-table (rest vars)))
       (map (lambda (env) (cons (binding (first vars) #f) env))
            (generate-truth-table (rest vars))))))

(define (is-tautology? expr)
  (foldr (lambda (x y) (and x y)) #t
         (map (lambda (env) (boolean-eval expr env))
              (generate-truth-table (get-variables expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Part I.  Negative Normal Form (NNF).
;  Contains function nnf? to check if a boolean expression is in NNF.
;  Contains function nnf and its helper function nnf-not to convert
;  a boolean expression into an equivalent expression that is in NNF.
;  The test suite nnf-suite is provided to test these functions.
;  The following property should be satisfied.
;  (and (is-nnf? (nnf expr)) (equal? (bool-eval expr env))
;                            (equal? (bool-eval (nnf expr) env)))
;        
;  This part of the assignment is provided as a model for the second part.JJ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;nnf?
;Input: a boolean expression
;Output: a boolean which is true if the input expression is in negative normal form.
(define (nnf? expr)
  (cond
   [ (constant? expr) #t ]
   [ (variable? expr) #t ]
   [ (negation? expr) (variable? (op1 expr)) ]
   [ (disjunction? expr) (and (nnf? (op1 expr)) (nnf? (op2 expr))) ]
   [ (conjunction? expr) (and (nnf? (op1 expr)) (nnf? (op2 expr))) ]
  ) 
)

;nnf - convert boolean expression to negative normal form.
;Input: A boolean expression
;Output: A boolean expression that is equivalent to the input and
;        is in negative normal form.
;        (and (nnf? (nnf expr)) (equal? (bool-eval expr env))
;                                  (equal? (bool-eval (nnf expr) env)))
(define (nnf expr)
  (cond
   [ (constant? expr) expr ]
   [ (variable? expr) expr ]
   [ (negation? expr) (nnf-not expr) ]
   [ (disjunction? expr) (list 'or (nnf (op1 expr)) (nnf (op2 expr))) ]
   [ (conjunction? expr) (list 'and (nnf (op1 expr)) (nnf (op2 expr))) ]
  ) 
)

;nnf-not.  Convert a negation into negative normal form.
;Input: A boolean expression that is a negation.
;Output: A boolean expression that is in NNF and equivalent to the input.
(define (nnf-not expr)
  (cond
   [ (constant? (op1 expr)) (not expr) ]
   [ (variable? (op1 expr)) expr ]
   [ (negation? (op1 expr)) (nnf (op1 (op1 expr))) ]
   [ (disjunction? (op1 expr)) (list 'and (nnf (list 'not (op1 (op1 expr))))
                                   (nnf (list 'not (op2 (op1 expr))))) ]
   [ (conjunction? (op1 expr)) (list 'or (nnf (list 'not (op1 (op1 expr))))
                                   (nnf (list 'not (op2 (op1 expr))))) ]
  )
)



(define-test-suite nnf-suite
(check-equal? 
(nnf? 'false) #t)
(check-equal? 
(nnf? 'x) #t)
(check-equal? 
(nnf? '(not x)) #t)
(check-equal? 
(nnf? '(and x (not y))) #t)
(check-equal? 
(nnf? '(and x (or y (not z)))) #t)
(check-equal? 
(nnf? '(not (not x))) #f)
(check-equal? 
(nnf? '(not (or x y))) #f)
(check-equal? (nnf 'x) 'x)
(check-equal? (nnf '(not x)) '(not x))
(check-equal? (nnf '(not (not x))) 'x)
(check-equal? (nnf '(not (or x y))) '(and (not x) (not y)))
(check-equal? (nnf '(not (and x y))) '(or (not x) (not y)))
(check-equal? (nnf '(not (or (and x (not y)) (not z))))
             '(and (or (not x) y) z))
)
(run-tests nnf-suite 'verbose)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Part II. Conjunctive Normal Form (CNF).
;  Contains function is-cnf? and its helper functions no-and? and no-or-above-and?
;  to check if a boolean expression is in CNF.
;
;  Contains function cnf and its helper function distrib-orand to convert
;  a boolean expression into an equivalent expression that is in CNF.
;  The test suites is-cnf-suite and cnf-suite are provided to test these functions.
;  The following property should be satisfied.
;  (and (cnf? (cnf expr)) (equal? (bool-eval expr env))
;                            (equal? (bool-eval (cnf expr) env)))
;        
;  Students must provide and test the following functions:
;  cnf?, no-and?, no-or-above-and?, cnf, and distrib-orand.
;
;  cnf? checks to see that an expression is in NNF using is-nnf?
;  and has no ors above ands using no-or-above-and?
;
;  no-or-above-and? recursively traverses a boolean expression checking
;  that there are no ors above ands.  When an or is encountered the
;  function no-and? is used to check that the operands to the or do
;  not contain any ands.
;
;  cnf calls nnf to convert the input expression to NNF and then calls
;  nnf2cnf to convert the resulting NNF expression to CNF.  After recursively
;  converting the operands to an or, nnf2cnf calls distrib-orand to
;  distribute the and over ors.
;  
;  When implementing cnf and cnf? first outline how they are to be
;  implemented using the helper functions and then implement and test the
;  helper functions before testing the top level cnf and cnf? functions.
;  cnf? can be used to help test cnf. JJ
;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;cnf? A boolean expression is in cnf if it is in nnf and there are not ors
;        above above ands
(define (cnf? expr)
  (and (nnf? expr) (no-or-above-and? expr))
)

;no-or-above-and?
(define (no-or-above-and? expr)
  (cond
   [ (constant? expr) #t ]
   [ (variable? expr) #t ]
   [ (negation? expr) (no-or-above-and? (op1 expr)) ]
   [ (disjunction? expr) (and (no-and? (op1 expr)) (no-and? (op2 expr))) ] 
   [ (conjunction? expr) (and (no-or-above-and? (op1 expr))
                         (no-or-above-and? (op2 expr))) ]
  )
)

;no-and? check if a boolean expression contains any ands.
(define (no-and? expr)
  (cond
   [ (constant? expr) #t ]
   [ (variable? expr) #t ]
   [ (negation? expr) (no-and? (op1 expr)) ]
   [ (disjunction? expr) (and (no-and? (op1 expr)) (no-and? (op2 expr))) ]
   [ (conjunction? expr) #f ]
  ) 
)

#| test suite for is-cnf?  Uncomment when you are ready to test is-cnf?
|#

(define-test-suite is-cnf-suite
(check-equal? 
  (cnf? 'x) #t)
(check-equal?
  (cnf? '(or x (or y z))) #t)
(check-equal?
  (cnf? '(or x (and y z))) #f)
(check-equal?
  (cnf? '(and x (or y z))) #t)
(check-equal?
  (cnf? '(and (or x (not y)) (and (or y (not z))
                 (or (not x) (or y z))))) #t)
(check-equal?
  (cnf? '(and (or x (not y)) 
                 (or (not x) (and y z)))) #f)
)
(run-tests is-cnf-suite 'verbose)


; cnf
; Input:  A boolean expression
; Output: A boolean expression in CNF that is equivalent to the input expression.
(define (cnf expr)
  (nnf2cnf (nnf expr)))

; nnf2cnf
; Input:  A boolean expression in NNF
; Output: A boolean expression in CNF that is equivalent to the input expression.
(define (nnf2cnf expr)
  (cond
   [ (constant? expr) expr ]
   [ (variable? expr) expr ]
   [ (negation? expr) expr ]
   [ (conjunction? expr) (list 'and (cnf (op1 expr)) (cnf (op2 expr))) ]
   [ (disjunction? expr) (distrib-orand (cnf (op1 expr)) (cnf (op2 expr))) ]
  )
)

; distrib-orand.  Distribute or over and.
; Input:  Two expressions in CNF
; Output: An expression in CNF equivalent to (or expr1 expr2)
; There are three cases.  Let expr1 = E1 and expr2 = E2
; 1) E1 = E11 /\ E12.  (E11 \/ E2) /\ (E12 \/ E2)
; 2) E2 = E21 /\ E22.  (E1 \/ E21) /\ (E1 \/ E22)
; 3) E1 and E2 contain no ands.  E1 \/ E2
; In the first two cases distrib-orand must be called recursively as
; there may be additional ands to distribute.
(define (distrib-orand expr1 expr2)
  (cond
    [(conjunction? expr1) (list 'and (distrib-orand (op1 expr1) expr2)
                                (distrib-orand (op2 expr1) expr2)) ]
    [(conjunction? expr2) (list 'and (distrib-orand expr1 (op1 expr2))
                                (distrib-orand expr1 (op2 expr2))) ]
    [ else (list 'or expr1 expr2) ]
  )
)


#| test suite for cnf  Uncomment when you are ready to test cnf.
|#
(define-test-suite cnf-suite
(check-equal?
   (distrib-orand 'x 'y) '(or x y))
(check-equal? 
  (distrib-orand '(and x y) 'z) '(and (or x z) (or y z)))
(check-equal? 
  (distrib-orand 'u '(and x y)) '(and (or u x) (or u y)))
(check-equal? 
  (cnf 'x) 'x)
(check-equal? 
  (cnf '(not x)) '(not x))
(check-equal? 
  (cnf '(not (not x))) 'x)
(check-equal? 
  (cnf '(and x y)) '(and x y))
(check-equal? 
  (cnf '(not (and x y))) '(or (not x) (not y)))
(check-equal? 
  (cnf '(or x y)) '(or x y))
(check-equal? 
  (cnf '(and (or x y) (or (not x) z))) '(and (or x y) (or (not x) z)))
(check-equal? 
  (cnf '(or (and x y) z)) '(and (or x z) (or y z)))
(check-equal? 
  (cnf '(or x (and y z))) '(and (or x y) (or x z)))
(check-equal? 
  (cnf '(or (and x (and y z)) w)) '(and (or x w) (and (or y w) (or z w))))
)
(run-tests cnf-suite 'verbose)


#|
Part III.  Equivalence testing.

The function cnf? allows you to check if the output of cnf is in fact in
conjunctive normal form.  We used this in our unit tests to check whether
the function cnf was correct in several examples.

We did not check to see if the result was equivalent to the initial expression.
In this part, we use functions from the tautology checker lab to verify this.
To verify that the boolean expressions E1 and E2 are equivalent, check to see
if E1 <-> E2 is a tautology.  Recall that E1 <-> E2 is equivalent to
(E1 -> E2) /\ (E2 -> E1).  Given this you can use the function is-tautology?
from the lab to check that E1 and E2 are equivalent.  For your convenience
the function is-tautology? is provided in Part 0. JJ

|#

(define (is-equivalent? expr1 expr2)
  (is-tautology? (list 'and (list 'implies expr1 expr2)
                            (list 'implies expr2 expr1))))
                                  
#| test suite for equivalence testing.
   Uncomment when you are ready to test cnf.
|#

(define-test-suite equiv-suite
(check-equal? 
  (is-equivalent? (cnf '(not (and x y)))
                     '(not (and x y))) #t)
(check-equal? 
  (is-equivalent? (cnf '(and (or x y) (or (not x) z)))
                     '(and (or x y) (or (not x) z))) #t)
(check-equal?
  (is-equivalent? (cnf '(or (and x y) z))
                     '(or (and x y) z)) #t)
(check-equal? 
  (is-equivalent? (cnf '(or x (and y z)))
                     '(or x (and y z))) #t)
(check-equal? 
  (is-equivalent? (cnf '(or (and x (and y z)) w))
                     '(or (and x (and y z)) w)) #t)
)
(run-tests equiv-suite 'verbose)
