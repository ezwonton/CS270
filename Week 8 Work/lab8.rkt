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
    [(odd? x) (println "Impossible")]
    [(= x 14) (list 2 1)]
    [(= x 12) (list 3 0)]
    [(= x 16) (list 1 2)]
    [else (let (
               (res (postage46 (- x 4))))
               (list (+ 1 (first res)) (second res))
               )]
  )
)

(postage46 18)
(postage46 20)
(postage46 21)




(define (GenDominoes n)
  (if (> n 0)
      (append (map (lambda(x) (string-append "V" x)) (GenDominoes (- n 1)))
              (if (> n 1) (map (lambda(x) (string-append "HH" x)) (GenDominoes (- n 2)))
              '()
              )
      )
      '("")
  )
)
(println "")
(GenDominoes 0)
(GenDominoes 1)
(GenDominoes 2)
(GenDominoes 3)
(GenDominoes 4)
(GenDominoes 5)
(GenDominoes 6)