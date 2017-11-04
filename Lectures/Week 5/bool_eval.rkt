#lang racket

(define (lookup var env)
(cond
  [(null? env) '()]
  [(equal? (first (first env)) var)
   (first env)]
  [else (lookup var (rest env))]
  ))
(define (global-lookup var world)
  (cond
  [(null? world) (error "Variable not in memory")]
  [else (let
       ( (result (lookup var (first world))) )
     (if (null? result)
         (global-lookup var (rest world))
         result))]
))
(define E  '( (a 5) (b 7) ) )
(lookup 'c E)
(define U '( ((a 5) (b 7)) ((c 8)) ((a 8) (t 9))) )
(global-lookup 't U)

(define example '(+ a 5))
(define (eval exp glob)
  (cond
    [(number? exp) exp]
    [(symbol? exp)
     (second (global-lookup exp glob))]
    [(equal? (first exp) '+)
     (+ (eval (second exp) glob)
        (eval (third exp) glob))]
    [(equal? (first exp) '*)
     (* (eval (second exp) glob)
        (eval (third exp) glob))]
  ))
(eval '5 U)
(eval 'c U)
(eval '(+ (* (+ 5 c) a) t) U)
;----------------------------------------------
(define (lookup_bool var env)
(cond
  [(null? env) (error "Var not found")]
  [(equal? (first (first env)) var)
   (second (first env))]
  [else (lookup_bool var (rest env))]
  ))
'( (a #t) (b #f) )


;Helper Functions
(define (variable? x)
  (and (symbol? x) (not (reserved-word x))))
(define (reserved-word x)
  (equal? 'AND x))
(define (constant? x)
  (or (equal? x '#t) (equal? x '#f)))
(define (and? x)
  (and (cons? x) (equal? (first x) 'AND)))
(define (or? x)
  (and (cons? x) (equal? (first x) 'OR)))
(define (not? x)
  (and (cons? x) (equal? (first x) 'NOT)))
(define (op1 x) (second x))
(define (op2 x) (third x))

(define (beval exp env)
  (cond
    [(constant? exp) exp]
    [(variable? exp) (lookup_bool exp env)]
    [(and? exp)
     (if (equal? (beval (op1 exp) env) #f)
         #f
         (beval (op2 exp) env))]
    [(or? exp) (or (beval (op1 exp) env)
                   (beval (op2 exp) env))]
    [(not? exp) (not (beval (op1 exp) env))]
    [else (error "Error!")]
  ))
(beval '(NOT (OR a #t)) '( (b #t) (a #t)))
;Tautology always true
;Contigent both true and false
;Contradictory always false
(define two_var_tt
  '( ((a #t) (b #t))
     ((a #t) (b #f))
     ((a #f) (b #t))
     ((a #f) (b #f))))
(define exp_to_test '(NOT (AND a b)))
(map
 (lambda (env) (beval exp_to_test env))
 two_var_tt)
(map
 (lambda (env) (beval '(OR (NOT a) (NOT b)) env))
 two_var_tt)






  