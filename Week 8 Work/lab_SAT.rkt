#lang racket


#|
First convert the expressions for sum and cout to CNF.
Then write a function FullAdder that generates input for MiniSAT in DIMACS format.
Use this function to generate the satisfiability problem corresponding to 3-bit addition.
Add clauses that specify a given binary sum and input a, and run MiniSAT to find the b, that satisfies a+b=sum.
|#