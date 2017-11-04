10/15/2017
12:00-21:00

10/16/2017
18:00-21:00

10/17/2017
16:00-21:00

10/21/2017
11:00-17:00

10/24/2017
16:00-16:30

Question 1-3: Peano
generally couldn't figure out the syntax
1. simple enough to figure out
2. had difficulty getting the right output
initial function was wrong and needed to be redone
3. most difficulty figuring this out as I had no idea how to check for a value less than n being the gcd

Questions 4-5: Binary
4. confused with what the given definitions implied
5. hard to figure out 3 x 3 case


Explanations
1. (sub m n)
if n is 0, output m
else recursively does the previous value of subtraction of the previous n from m until n is 0

2. (div m n)
if n is 0, output 0
elseif m is less than m, output zero
else take the successor of the m - n divided by n

2. (rem m n)
if n is 0, output 0
else subtract the product of n and the quotient of m divided n, from m

3. (gcd m n)
if n is 0, output m
else take the gcd of n and the remainder of m and n



for questions 4 & 5, I made a “previous” function
if b is 1, output 0
if b is double, output double-plus1 of op b
if b is double-plus1, output double of op b

4. (binplus a b)
if a is 0, outputs b
if b is 0, outputs a
else adds the previous a with the increment of b until a is 0 to output b

5. (binmult a b)
if a or b is 0, outputs 0
if a is 1, outputs b
if a is double, doubles the binmult of op a and b
else adds b to the binmult of previous a and b until a is 0
