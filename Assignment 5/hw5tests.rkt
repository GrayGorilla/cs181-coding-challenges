#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")

   ;; My tests
   (check-equal? (eval-exp (racketlist->mupllist (list 1 2 3 4 5)) (apair 1 (apair 2 (apair 3 (apair 4 (apair 5 (munit)))))))
                 "racketlist->mupllist test")
   (check-equal? (eval-exp
                  (mlet "x" (add (int 2) (int 3))
                        (ifnz (isgreater (var "x") (int 0))
                              (add (int 4) (add (int 1) (var "x")))
                              (add (int 2) (var "x")))))
                 (int 10)
                 "mlet & isgrader expression test")
   (check-equal? (eval-exp (mlet "doubleFunc" (fun null "x" (add (var "x") (var "x")))
                  (call "doubleFunc" (int 2))))
                 (int 4)
                 "function call test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
