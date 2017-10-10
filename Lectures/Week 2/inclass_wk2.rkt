#lang racket
(define e1 '(1 2 3 4 (+ 4 5)))
(display e1)
(display "\n")
(define e2 (list 1 2 3 4 (+ 4 5) e1))
(display e2)
(display "\n")

(null? '())
(null? '(1 2 3 4))
(null? 28)

(cons? '(1 2 3 4))
(cons? '())
(cons? 28)

(list? '(1 2 3 4))
(list? 28)
(display e1)
(display "\n")
(first e1)
(rest e1)

(cons 3 (cons 1 '()))
(cons 8 (cons 3 4))


(define (list_1? L) (or (null? L) (cons? L)))
(list_1? '(1 2 3))
(list_1? '())
(list_1? 28)
(list_1? (cons 1 2))
(list? (cons 1 2))

(define (list_2? L)
  (if (null? L) #t
      (if (cons? L)
          (list_2? (cdr L))
          #f
      )))
(display "List 2 Tests here!\n")
(list_2? '(1 2 3))
(list_2? '())
(list_2? 28)
(list_2? (cons 1 2))
(list? (cons 1 2))

(define (size L)
  (if (null? L)
      ;Base Case
      0
      ;Recursive case
      (+ 1 (size (rest L)))
  ))

(size '(1 2 3 4 5 ))

;(size '(1 2))
;(+ 1 (size '(2)))
;(+ 1 (+ 1 (size '())))
;(+ 1 (+ 1 0))
;2

(define (size2_helper L s)
  (begin
    (display "(size2_helper ")
    (display L)
    (display " ")
    (display s)
    (display ")\n")
  (if (null? L)
      s
      (size2_helper (rest L) (+ 1 s))
  )
  )
)
(define (size2 L) (size2_helper L 0))
(size2 '(1 2 3 4))


(define (member x L)
  (cond
    [(not (list? L)) #f]
    [(null? L) #f]
    [(equal? (first L) x) #t]
    [else (member x (rest L))]
  ))
(member 1 '(1 2 3))
(member 2 '(1 2 3))
(member '(1 2) '((3 4) (1 2) (7 8)))
(member 7 '(1 2 3))
(member 7 28)

(define (merge X Y)
  (cond
    [(null? X) Y]
    [else (cons (first X) (merge (rest X) Y))]
    )
)
(define y '(4 5 6))
(merge '(1 2 3) y)
(display y)(display "\n")


(define (reverse1 L)
  (if (null? L) L
      (merge
       (reverse1 (rest L))
       (cons (first L) '())
       )
  )
)
(reverse1 '(1 2 3))

(define (reverse2 input result)
  (begin
    (display "(reverse2 ")
    (display input)
    (display " ")
    (display result)
    (display ")\n")
  (if (null? input) result
      (reverse2 (rest input)
                (cons (first input) result)))
  )
)
(reverse2 '(1 2 3 4) '())
(define (reverse3 L) (reverse2 L '()))
(reverse3 '(1 2 3 4))


(define deep '((1 2) (3 (4)) (5 6)))
(display deep)
(display "\n")

(define (atom? x)
  (not (cons? x))
)
(define (num_of_atoms L)
  (cond
    [(null? L) 0]
    [(atom? L) 1]
    [(cons? L) ;(+ (atoms in item one) rec)
     (+ (num_of_atoms (first L))
        (num_of_atoms (rest L)))]
  )
)
(num_of_atoms deep)
(define (order L)
  (cond
    [(null? L) 0]
    [(atom? L) 0]
    [(cons? L)
     (max (+ 1 (order (first L)))
          (order (rest L)))
     ]
))
(order deep)

;(map f L) apply f to every element in L
(define (sqr x) (* x x))
(map sqr '(1 2 3 4))
(map (lambda (x) (* x x x)) '(1 2 3 4))
(map (lambda (x) (string-append x "!"))
     '( "Cat" "Bat" "Yellow"))

;(foldr f init L)
(foldr + 0 '(1 2 3 4))
(+ (+ (+ (+ 0 1) 2) 3) 4)
(foldr + 0 '(1))
(foldr * 1 '(1 2 3 4 5 6))

(define (flatten L)
  (cond [(null? L) L]
        [(atom? L) (list L)]
        [(cons? L)
         (foldr append '()
                (map flatten L))]
        )
)
(flatten deep)

(define (filter f L)
  (cond [(null? L) '()]
    [(f (first L))
     (cons (first L) (filter f (rest L)))]
    [else (filter f (rest L))]))
(filter odd? '(1 2 3 4 5 6 7 8 9 10))
(filter even? '(1 2 3 4 5 6 7 8 9 10))












(define (and x y) (if x y #f))
(and #t #t)
(and #t #f)
(and #f #t)
(and #f #f)

