#lang racket

(define (cube x)
  (* x (* x x))
)
(cube 4)
(define cube2 (lambda (x) (* x x x)))
(cube2 4)

(define a 7)
(cube a)

(define (cube3 x)
  (begin
    (display "cube3 ")
    (display x)
    (display "\n")
    (* x x x)
  ))

(cube3 100)

;kjsdhfkjhsd
;lkshdflhsd
#|

5! = 1*2*3*4*5
6! = 1*2*3*4*5*6
6! = 5! * 6
1! = 1
0! = 1

int fact(n)
{
    total=1;
    for(int i=1; i <=n; i++)
    {
      total = total * i;
    }
    return total;
}

|#

(define (fact_h i total n)
  (if (> i n)
	total
  (fact_h (+ i 1) (* total i) n)
))
(define (fact n) (fact_h 1 1 n))
(fact 5)

(require rackunit)
(require rackunit/text-ui)


(define-test-suite fact_test
  (check-equal? (fact 1) 1)
  (check-equal? (fact 2) (* 1 2))
  (check-equal? (fact 3) (* 1 2 3))
)
(run-tests fact_test)



(define thingy '(1 2 3 4 5 6))
(define (size L)
  (begin (display "(size ")
         (display L)
         (display ")\n")
  (if (null? L)
      0
      (+ 1 (size (rest L))))
))
(size thingy)


(define (get_n n L)
  (cond
    [(null? L) (display "List Empty!")]
    [(= n 0) (first L)]
    [else (get_n (- n 1) (rest L))]
  )
)
(get_n 3 thingy)
(get_n 100 thingy)



