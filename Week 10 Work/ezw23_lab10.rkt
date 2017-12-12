#lang racket

; Input: (and (arith-expr? expr1) (arith-expr? expr2))
; Output: (arith-expr? (plus-simp expr1 expr2)) which is equivalent to expr
(define (plus-simp expr1 expr2)
  (cond
    [ (and (constant? expr1) (constant? expr2)) (+ expr1 expr2) ] [ (equal? expr1 0) expr2 ]
    [ (equal? expr2 0) expr1 ]
    [ (make-plus expr1 expr2) ]
  )
)


; Input: (and (arith-expr? expr1) (arith-expr? expr2))
; Output: (arith-expr? (mult-simp expr1 expr2)) which is equivalent to expr
(define (mult-simp expr1 expr2)
  (cond
    [ (and (constant? expr1) (constant? expr2)) (* expr1 expr2) ] [ (equal? expr1 0) 0 ]
    [ (equal? expr2 0) 0 ]
    [ (equal? expr1 1) expr2 ]
    [ (equal? expr2 1) expr1 ]
    [ else (make-mult expr1 expr2) ]
  )
)


; Input: (arith-expr? expr)
; Output: (arith-expr? (arith-simp expr))equivalent to expr
(define (arith-simp expr)
  (cond
    [(constant? expr) expr]
    [(variable? expr) expr ]
    [(plus? expr) (let ([simpexpr1 (arith-simp (op1 expr))] [simpexpr2 (arith-simp (op2 expr))])
                     (plus-simp simpexpr1 simpexpr2))]
    [(mult? expr) (let ([simpexpr1 (arith-simp (op1 expr))] [simpexpr2 (arith-simp (op2 expr))])
                     (mult-simp simpexpr1 simpexpr2))]
  )
)


(define (is-simplified? expr)
  (if (constant? expr)
      #t
      (and (noconstant-arith? expr) (nozeros? expr) (nomult1? expr))))


(define (noconstant-arith? expr)
  (cond
   [ (constant? expr) #t ]
   [ (variable? expr) #t ]
   [ (plus? expr) (if (and (constant? (op1 expr)) (constant? (op2 expr)))
                      #f
                      (and (noconstant-arith? (op1 expr)) (noconstant-arith? (op2 expr)))) ]
   [ (mult? expr)  (if (and (constant? (op1 expr)) (constant? (op2 expr)))
                      #f
                      (and (noconstant-arith? (op1 expr)) (noconstant-arith? (op2 expr)))) ]
   [else #f ]
  )
)


(define (nozeros? expr)
  (cond
    [(constant? expr) (not (equal? expr 0))]
    [(variable? expr) #t]
    [(plus? expr) (and (nozeros? (op1 expr))
                        (nozeros? (op2 expr)))]
    [(mult? expr) (and (nozeros? (op1 expr))
                        (nozeros? (op2 expr)))]
    )
)


(define (nomult1? expr)
  (cond
    [(constant? expr) #t]
    [(variable? expr) #t]
    [(plus? expr) (and (nomult1? (op1 expr)) (nomult1? (op2 expr)))]
    [(mult? expr) (if (or (equal? (op1 expr) 1) (equal? (op2 expr) 1))
                       #f
                       (and (nomult1? (op1 expr)) (nomult1? (op2 expr))))]
    )
)