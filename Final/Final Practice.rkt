#lang racket



; Binary Trees for Final Exam Practice


; Define the following access functions to access the left and right children and value of a node.
(define (left T) (second T))
(define (right T) (third T))
(define (value T) (first T))



; The number of nodes in a binary tree can be computed recursively
(define (N T)
  (if (null? T)
      0
      (+ 1 (N (left T)) (N (right T)))
  )
)



; The height of a binary tree is the longest path from the root to a leaf node. It can be computed recursively as
(define (H T)
  (if (null? T)
      -1
      (+1 (max (H (left T))) (H (right T)))
  )
)



; Write a Racket function to construct a complete binary tree of a given height.



