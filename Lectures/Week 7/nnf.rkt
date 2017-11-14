#lang racket
;Mark Boady
;CS 270 Negative Normal Form Example
(require rackunit)
(require rackunit/text-ui)
(define example '(not (not (not (and A (not (or B (not C))))))))
(display example)

;Helper Functions
(define (op1 x) (second x))
(define (op2 x) (third x))
;Check Operators
(define (is_not x)
  (and (list? x)
       (equal? (length x) 2)
       (equal? (first x) 'not)))
(define (is_and x)
  (and (list? x)
       (equal? (length x) 3)
       (equal? (first x) 'and)))
(define (is_or x)
  (and (list? x)
       (equal? (length x) 3)
       (equal? (first x) 'or)))
;NNF Tests
(define (is_double_neg x)
  (and (is_not x) (is_not (op1 x))))
(define (is_demorgan_or x)
  (and (is_not x) (is_or (op1 x))))
(define (is_demorgan_and x)
  (and (is_not x) (is_and (op1 x))))

;To NNF
(define (nnf expr)
  (cond
    [(is_double_neg expr) (nnf (op1 (op1 expr)))]
    [(is_demorgan_or expr) (nnf (list 'and
                                 (list 'not (op1 (op1 expr)))
                                 (list 'not (op2 (op1 expr)))))]
    [(is_demorgan_and expr) (nnf (list 'or
                                 (list 'not (op1 (op1 expr)))
                                 (list 'not (op2 (op1 expr)))))]
    [(is_not expr) (list 'not (nnf (op1 expr)))]
    [(is_and expr) (list 'and (nnf (op1 expr)) (nnf (op2 expr)))]
    [(is_or expr) (list 'or (nnf (op1 expr)) (nnf (op2 expr)))]
    [else expr]
  )
)

(display "\nConvert to NNF\n")
(nnf '(not (not A)))
(nnf '(not (or A B)))
(nnf '(not (and A B)))
(nnf '(not (not (not A))))
(nnf '(not (and (not A) (not B))))

(define-test-suite nnf-suite
(check-equal? (nnf 'x) 'x)
(check-equal? (nnf '(not x)) '(not x))
(check-equal? (nnf '(not (not x))) 'x)
(check-equal? (nnf '(not (or x y))) '(and (not x) (not y)))
(check-equal? (nnf '(not (and x y))) '(or (not x) (not y)))
(check-equal? (nnf '(not (or (and x (not y)) (not z))))
             '(and (or (not x) y) z))
)
(run-tests nnf-suite 'verbose)
(nnf example)