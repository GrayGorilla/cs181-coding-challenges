
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define (sequence spacing low high)
  (cond [(= low high) (list low)]
        [(> low high) (list)]  ; empty list
        [#t (cons low (sequence spacing (+ low spacing) high))]))
