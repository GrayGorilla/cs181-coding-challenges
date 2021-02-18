
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define (sequence spacing low high)
  (if (= low high)
    (list low)
    (if (> low high)
        (list)  ;; empty list
        (cons low (sequence spacing (+ low spacing) high)))))
