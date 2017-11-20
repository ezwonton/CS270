#lang racket


(define (postage45 x)
  (cond
    [(<= x 11) (error "Impossible")]
    [(= x 12) (list 3 0)]
    [(= x 13) (list 2 1)]
    [(= x 14) (list 1 2)]
    [(= x 15) (list 0 3)]
    [else (let (
               (res (postage45 (- x 4))))
               (list (+ 1 (first res)) (second res))
               )]
  )
)

(postage45 19)
(postage45 21)

(define (postage46 x)
  (cond
    [(<= x 11) (error "Impossible")]
    [(= x 12) (list 0 2)]
    [(= x 13) (error "Impossible")]
    [(= x 14) (list 1 1)]
    [(= x 15) (error "Impossible")]
    [(= x 16) (list 1 2)]
    [(= x 17) (error "Impossible")]
    [else (let (
               (res (postage45 (- x 4))))
               (list (+ 1 (first res)) (second res))
               )]
  )
)

(postage46 19)
(postage46 21)
