#lang racket


;Part I
(display "\nPart I \n")
(+ (* 2 3) (- 4 5))
(list 1 2 3)
(list (- 4 3) (- 4 2) (- 4 1))
'(1 2 3)
'(list (- 4 3) (- 4 2) (- 4 1))
#(1 2 3)
null
'()
(cons 1 null)
(cons 1 (cons 2 null))
(cons 1 (cons 2 (cons 3 null)))
(first (list 1 2 3))
(second (list 1 2 3))
(third (list 1 2 3))
(rest '(1 2 3))
(first (rest '(1 2 3)))
(rest (rest '(1 2 3)))
(first (rest (rest '(1 2 3))))
(if (= (length '(1 2 3)) 3) "ok" "not ok")



;Part II
(display "\nPart II \n")
(define (add1 x)(+ x 1))
(add1 3)

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1) )
      )
  )
)
(fact 5)
(fact 6)



