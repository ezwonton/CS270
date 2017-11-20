#lang racket
(define (postage x)
  (cond
    [(<= x 11) (error "Impossible!")]
    [(= x 12) (list 3 0)]
    [(= x 13) (list 2 1)]
    [(= x 14) (list 1 2)]
    [(= x 15) (list 0 3)]
    [else (let
              ( (res (postage (- x 4))) )
            (list (+ 1 (first res)) (second res)))]
))