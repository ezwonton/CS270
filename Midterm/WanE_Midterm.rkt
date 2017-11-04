#lang racket
; Eric Wan - ezw23@drexel.edu

; Question 2
(define (unknown L)
  (cond
    [(null? L) (error "empty list")]
    [(null? (rest L)) (first L)]
    [else (unknown (rest L))]))

(unknown '(1 2 3))
(unknown '(1))

(cons '() (cons '() '()))


; Question 3
(define (member? x S)
  (cond
    [(null? S) #f]
    [(equal? x (first S)) #t]
    [else (member? x (rest S))]))

(member? 'y '(x y))


(define (Union list1 list2)
  (cond
    [(null? list1) list2]
    [(null? list2) list1]
    [else (if (member? (first list1) list2)
              (Union (rest list1) list2)
              (cons (first list1) (Union (rest list1) list2)))]
  )
)

(Union '() '(1 2 3))
(Union '(1 2 3) '())
(Union '(1 2 5) '(1 2 3))


; Question 4

(define (charfun pred?)
  (cond
    [(null? pred?) 0]
    [else (if (and #t pred?)
              1
              0)]
  )
)

; (foldr + 0 (map (lambda(x) (if (and #t x) 1 0)) (map (lambda (list)(predicateprime? list))(range 1 100))))
#|
the function predicateprime? does not work
|#


(append '(3 4) '(1 2))
(define (appendfoldr list1 list2)
  (cond
    [(null? list1) list2]
    [(null? list2) list1]
    [else (foldr cons list2 list1)]
  )
)
(appendfoldr '(3 4) '(1 2))


; Question 5
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


(define (iff? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (first expr) 'iff)
  )
)

(define (remove-iff expr)
    (cond
    [(constant? expr) expr]
    [(variable? expr) expr]
    [(negation? expr) expr]
    [(disjunction? expr) expr]
    [(conjunction? expr)  expr]
    [(implication? expr)  expr]
    [(iff? expr) (and (implies (op1 expr)(op2 expr)) (implies (op2 expr)(op1 expr)))]
    ))

