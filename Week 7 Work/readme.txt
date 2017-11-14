Eric Wan - ezw23@drexel.edu

2017/11/08
20:30 - 21:00

2017/11/11
10:30 - 12:00
16:40 - 21:30

2017/11/13
17:00 - 22:00

2017/11/14
16:00 - 17:00


Part II
created is-nnf? function to check if the given expression is in negative normal form.

function cnf? is an and checking if the given expression is in nnf form and if the expression has no or above and.

function no-or-above-and? checks if the expression given is a variable or a negation and outputs true if the expression is one of the two. it also has to check an and/or leading the expression. if the expression is an and, it checks the parts of the and for no-or-above-and? recursively. if the expression is an or, it checks if the parts of the or have no and tailing it. 

function no-and? checks if the expression given is a variable or a negation and outputs true if either case is true. it checks if the expression is an or, and recursively calls no-and? on the boolean function of the rest of the expression.

function cnf calls the nnf2cnf of nnf of the expr.

function nnf2cnf checks if the expr is a variable, outputs the expr. it checks if the expr is a negation, outputs the nnf of the expr. it checks if the expr is an and, outputs a list of and with the recursive calls of the two parts of the and expression. it checks if the expr is an or and checks if there is no-or-above-and? and outputs the expr if so, else it distributes or over and over the two parts of the expr.

function distrib-orand is checks if both parts of the expression are ands. if so, it distributes out the and into a list with a recursive call to check if the inside parts of the expressions are also and. else it creates a list of or with the expressions

function is-equivalent? checks if the tautology checks of both expressions are the same.