#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


;; 1
(define (sequence spacing low high)
  (cond [(= low high) (list low)]
        [(> low high) (list)]  ; empty list
        [#t (cons low (sequence spacing (+ low spacing) high))]))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (quote (error "list-nth-mod: negative number"))]
        [(= (length xs) 0) (quote (error "list-nth-mod: empty list"))]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4
(define (stream-for-k-steps s k)
  (if (= k 0)
      (list)
      (cons (car (s)) (stream-for-k-steps (cdr (s)) (- k 1)))))

;; 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 6) 0)
                    (cons (- 0 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; 6
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (= (remainder x 2) 0)
                    (cons "dan.jpg" (lambda () (f (+ x 1))))
                    (cons "dog.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 0))))

;; 7
(define (stream-add-one s)
  (letrec ([f (lambda (strm)
                (cons (cons 1 (car (strm))) (lambda () (f (cdr (strm))))))])
    (lambda () (f s))))

;; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; 9
(define (cycle-lists-challenge xs ys)
  (letrec ([f (lambda (i j)
               (cond [(and (null? (list-tail xs (+ i 1))) (null? (list-tail ys (+ j 1))))
                      (cons (cons (car (list-tail xs i)) (car (list-tail ys j))) (lambda () (f 0 0)))]
                     [(null? (list-tail xs (+ i 1)))
                      (cons (cons (car (list-tail xs i)) (car (list-tail ys j))) (lambda () (f 0 (+ j 1))))]
                     [(null? (list-tail ys (+ j 1)))
                      (cons (cons (car (list-tail xs i)) (car (list-tail ys j))) (lambda () (f (+ i 1) 0)))]
                     [#t (cons (cons (car (list-tail xs i)) (car (list-tail ys j))) (lambda () (f (+ i 1) (+ j 1))))]))])
    (lambda () (f 0 0))))
