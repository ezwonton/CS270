assign1sol - Eric Wan - CS270-003

10/03/2017
18:00-19:00, 20:00-23:00
questions 1-8
difficulty with making or logic (understanding the syntax and how it affects the output)
difficulty with making implies (understanding the syntax and how it affects the output)
question 9 - not sure how to use foldr
question 10 - not sure how to do the list indexing
stopped here

10/07/2017
11:00 - 14:30
question 10/11
fixing the syntax for conditional statements (helped quickly finish 12 & 13)

10/11/2017
20:00 - 21:00
question 10-13
not fully understanding the idea of recursion


Q1: Creates logical not
if given no input, outputs message indicating that
if input is TRUE, outputs false
else it outputs FALSE

Q2: Creates logical and
if given no inputs, outputs message indicating which one is not given
if equal, outputs one of the inputs (#F#F -> #F and #T#T -> #T)
else it outputs FALSE

Q3: Creates logical or
if given no inputs, outputs message indicating which one is not given
checks if the first input is false and if the second input is false
then checks if the above two cases are equal
then checks if the above case is equal to TRUE and outputs one of the inputs
else it outputs TRUE

Q4: Creates logical xor
if given no inputs, outputs message indicating which one is not given
checks if the first input is false and if the second input is false
then checks if the above two cases are equal
then checks if the above case is equal to TRUE and outputs FALSE
else it outputs TRUE

Q5: Creates logical implies
if given no inputs, outputs message indicating which one is not given
checks if the first input is false and if the second input is true
then checks if the above two cases are equal
then checks if the above case is equal to TRUE and outputs the second input
else it outputs TRUE

Q6: Creates logical iff
if given no inputs, outputs message indicating which one is not given
checks if the first input is false and if the second input is false
then checks if the above two cases are equal
then checks if the above case is equal to TRUE and outputs TRUE
else it outputs FALSE

Q7: Checks if (iffi e1 e2) = (andi (impliesi e1 e2) (impliesi e2 e1))
checks every combinations of e1 and e2 inputs for equality

Q8: Checks if (implies e1 e2) = (ori (noti e1) e2)
checks every combinations of e1 and e2 inputs for equality

Q9: Creates andlist and orlist
andlist iterates through all boolean values of a list checking against an initial TRUE
orlist iterates through all boolean values of a list checking against an initial FALSE

Q10: Creating a function checking list for all ones
if given no list, outputs FALSE
converts all 1s in list to TRUE and others to False
runs andlist over the edited boolean list to check if it consists of all ones
edit: if first value of list is 1, recalls function until null is produced equating to true, else false implying there is a 0


Q11: Creating a function checking list for at least one “1”
if given no list, outputs FALSE
converts all 1s in list to TRUE and others to False
runs orlist over the edited boolean list to check if it consists of at least one “1”
edit: if first value of list is 0, recalls function until null is produced equating to false, else true implying there is a 1


Q12: Creating a function checking list for exactly one “1”
if given no list, outputs FALSE
if the summation of the members of the list equals 1, outputs TRUE else output FALSE
edit: if first value of list is 0, recalls function until null is produced equating to false. if the first value is a 1, including from the recursive calling, checks if the rest of the list is null, if so then there is only 0’s remaining and true, else false.


Q12: Creating a function checking list for odd number of “1”
if given no list, outputs FALSE
if the summation of the members of the list is odd, outputs TRUE else output FALSE
edit: if the first value of list is 0, recalls function until null is produced. if the first value is 1, recalls the function and not’s it until a null is produced



