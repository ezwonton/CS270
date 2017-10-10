#lang racket
;Mark Boady
;Drexel University
;CS 270

;Lists and Recursion

;Basic List Operations
;A list is a list of elements
;Two ways to create a list
;Notice that the list command evaluates the elements but the ' does not.
(define example1 '(1 2 3 4 (+ 3 2)))
(define example2 (list 1 2 3 4 (+ 3 2)))
(display example1)
(display "\n")
(display example2)
(display "\n")

;There are three boolean functions to learn about lists
(display "Testing Boolean Functions\n")
;Null is True when the list is EMPTY and False Otherwise
(display "null?\n")
(null? '())
(null? '(1 2 3))
(null? 7) ; This is false. 7 is not the null list.
;Cons is True when the list was created using a cons.
;This means it has at least one element
(display "cons?\n")
(cons? '())
(cons? '(1 2 3 4))
(cons? 8); This is false. It is not a list with > 0 elements
;List is True if the input is a list.
(display "list?\n")
(list? '())
(list? '(1 2 3 4))
(list? 7)

;There are three primary functions for list manipulation
(display "List Manipulation\n")
;First gets the first element of the list.
(display "First\n")
(first '(1 2 3 4 5))
(first '((1 2) (3 4)))
;Rest returns a list with the first element removed
(display "Rest\n")
(rest '(1 2 3 4))
(rest '(1 2))
(rest '((1 2) (3 4)))
(rest '(1))
;Cons takes an item and a list and adds the item to the beginning of the list
(display "Cons\n")
(cons 1 '())
(cons 2 '(1))
;Notice this is not an append
(cons '(1 2) '(3 4))

;Terminology
;first is also called car
;car - Contents of Address Register
;rest is also called cdr
;cdr - Contents of Decrement Register
;The list is a set of nodes spanning two registers.
;car is a value and cdr is a point to the rest of the list.

;Pairs
(display "Pairs\n")
;You can put a value into the "pointer" part of the node.
;This creates a pair.
(define pair1 (cons 1 2))
(display pair1)
(display "\n")
;A has exactly 2 items.
;You need to use car/cdr to work with pairs
;first/rest will do type checking and cause an error
(car pair1)
(cdr pair1)
;If you see a dot in your list, it means you accidently made a pair.
;Make sure the second input to cons is a list.

;Lists are defined recursively
;List : Null | (cons All List)

(display "My List? Function\n")
;We can make our own list?
(define (my_list? L)
  (if (null? L)
      #t
      (if (cons? L)
          (my_list? (cdr L));cdr vs rest - do we want false or error on pair?
          #f
      )
  )
)
(my_list? '())
(my_list? '(1 2 3 4))
(my_list? (cons 1 2))
(my_list? 7)

(display "My List? With Boolean Expressions\n")
(define (my_list2? L)
  (or (null? L) (and (cons? L) (my_list2? (rest L))))
)
(my_list? '())
(my_list? '(1 2 3 4))
(my_list? 7)

(display "Determine the Size (length) of a list.\n")
(define (size L)
  (if (null? L)
      0
      (+ 1 (size (rest L)))
  )
)
(size '())
(size '(1))
(size '(1 (2 3)))
(size '(1 2 3))
(size '(1 2 3 4 5 6 7))

(display "Determine is x in list L with member\n")
(define (member x L)
  (cond
    [(null? L) #f];Not found
    [(equal? x (first L)) #t];Found. equal? works on any type
    [else (member x (rest L))];keep looking
  )
)
(member 1 '())
(member 1 '(1 2 3 4))
(member 7 '(1 2 3 4 5 6))
(member 7 '(1 2 3 7 4 5 6))
(member '(1 2) '(1 2 3 (1 2) 4))
(member 4 '(1 2 3 4))

(display "Append (merge) two lists.\n")
(define (merge X Y)
  (if (null? X)
      Y
      (cons (first X) (merge (rest X) Y))
   )
)
(merge '(1 2 3) '(4 5 6))
(merge '() '(1 2 3))
(merge '(1 2 3) '())
(merge '(1 (2 3) 4) '(a b c d))

;Two Versions of Reverse
(display "Reverse using append\n")
(define (reverse1 L)
  (if (null? L)
      '()
      (append (reverse1 (rest L)) (cons (first L) '()))
   )
)
;Notice: The second input of append needs to be a LIST
;We need to make (first L) into a list.
(reverse1 '())
(reverse1 '(1 2 3 4))
(reverse1 '(4 3 2 1))
(reverse1 '(a b c d))

;Tail Recursion
;If we use the return value of a recursive function
;the computer needs to keep the function call stack.
;That ways it knows what to do.

;Tail Recursion is more efficient.
;If everything is done in the function inputs,
;the call stack is not required.
;The computer can discard that memory.

;Tail Recursive Reverse requires two functions.
;This is common in tail recursion.
;We need to preset default function inputs using a second
;function.
(display "Reverse with Tail Recursion\n")
(define (reverse2-helper X Y)
  (if (null? X)
      Y
      (reverse2-helper (rest X) (cons (first X) Y))
  )
)
(define (reverse2 X)
  (reverse2-helper X '())
)

(reverse2 '())
(reverse2 '(1 2 3 4))
(reverse2 '(4 3 2 1))
(reverse2 '(a b c d))

(display "Reverse Tail Recursion with debugging\n")
(define (reverse3-helper X Y)
  (begin
    (display "(reverse3-helper ")
    (display X)
    (display " ")
    (display Y)
    (display ")\n")
    ;Actual Code
     (if (null? X)
         Y
         (reverse3-helper (rest X) (cons (first X) Y))
      )
  );end of the begin
)
(define (reverse3 X)
  (reverse3-helper X '())
)
(reverse3 '(4 3 2 1))

;The lists we have worked with up to this point only have a depth of 1
;We don't care if a list has a nested list like '(1 (2 3) 4)
;What if we do care about that??

;An atom is a object that cannot be broken down any further.
;The number of atoms in a list is the count of actual values.
; '(1 (2 3) 4) has 4 values but a lenght of 3

(display "Count Number of Atoms\n")
;An atom cannot be broken down.
(define (atom? x)
  (not (cons? x))
)

(define (num_of_atoms X)
  (cond
    [(null? X) 0]
    [(atom? X) 1]
    [(cons? X)
     (+ ;Number of atoms in my first element
      (num_of_atoms (first X))
      ;+ Number of atoms in rest of list
      (num_of_atoms (rest X))
     )
    ]
  )
)

(num_of_atoms '(1 (2 3) 4))
(num_of_atoms '(1 (2 3 4) ((((5)))) (6 (7 (8 (9 (10)))))))

;num_of_atoms is a deep recursion function
;It looks into nested lists
;Deep Recursion: Looks into all nested lists
;Recursion on both first and rest
;Shallow Recursion: Only Looks at top most level of list
;Recursion on rest only

;Deep Recrusion Example 2
;What is the max depth of a list?
(display "Find the Max Depth of a List.\n")
(define (order X)
  (cond
    [(null? X) 0]
    [(atom? X) 0]
    [(cons? X)
     (max
      (+ 1 (order (first X)))
      (order (rest X))
      )
    ]
  )
)
(order '())
(order '(1 2 3))
(order '(1 (2 3) 4))
(order '(1 (2 3 4) ((((5)))) (6 (7 (8 (9 (10)))))))

;High Order Functions
;Functions that take other functions are called high-order
;Two of the most common are map and foldr

(display "Map Examples\n")
;(map function list) returns list with function applied to every element
(define (sqr x) (* x x))
(map sqr '(1 2 3 4 5))
(map (lambda (x) (* x x x)) '(1 2 3 4 5))

;(foldr function initial list) merges all the elements in a list
;(foldr + 0 '(1 2)) computes (+ (+ 0 1) 2)
;Since the function takes to inputs, but the list might only have 1,
;we need an initial value.
(foldr + 0 '(1 2 3 4 5))
(foldr * 1 '(1 2 3 4 5))

;Flatten a deeply nested list using map/foldr
(display "Flatten a deeply nested list\n")
(define (flatten X)
  (cond
    [(null? X) X]
    [(atom? X) (list X)]
    [(cons? X)
     ;Use append to merge lists
     (foldr append '()
            ;Recursively flatten the rest of the
            ;List
            (map flatten X)
      )
   ]
 )
)
(flatten '(1 (2 3 4) ((((5)))) (6 (7 (8 (9 (10)))))))

;Fun with high level functions
;filter - take a boolean function and only keep elements that return true
(display "Filter A List?\n")
(define (filter f L)
  (cond
    ;Nothing to filter
    [(null? L) '()]
    ;Element should stay
    [(f (first L)) (cons (first L) (filter f (rest L)))]
    ;Element should go
    [else (filter f (rest L))]
  )
)
(filter even? '(1 2 3 4))
(filter odd? '(1 2 3 4))

;Count to a number starting at 0 and going by 1
(display "Generate a List\n")
(define (count_to stop)
  (count_to_helper 0 stop)
)
(define (count_to_helper start stop)
  (if (<= start stop)
      (cons start (count_to_helper (+ start 1) stop))
      '()
  )
)
(count_to 20)

;Put them together
;Get every even number between 0 and 25
(display "Generate a List of Even Numbers.\n")
(filter even? (count_to 25))

(display "Generate a List of Even Cubes.\n")
(filter even? (map (lambda (x) (* x x x)) (count_to 25)))


;Let - A useful command for complex functions
;the let command allows you to use names to replace values in a function
(display "Let Example. Find Min in list\n")
(define (list_min L)
  (if (null? L)
      +inf.0 ;infinity is a float
  (let
      (
        (A (first L))
        (B (list_min (rest L)))
      )
      (min A B)
  )
  )
)
(list_min '(5 2 34 5 3 1 3))

