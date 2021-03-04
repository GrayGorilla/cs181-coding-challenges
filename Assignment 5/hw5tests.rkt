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

   ;(check-equal? (mupllist->racketlist
   ;               (eval-exp (call (call mupl-all-gt (int 9))
   ;                               (racketlist->mupllist 
   ;                                (list (int 10) (int 9) (int 15))))))
   ;              (list (int 10) (int 15))
   ;              "provided combined test using problems 1, 2, and 4")

   ;; My tests
   (check-equal? (racketlist->mupllist (list 1 2 3 4 5))
                 (apair 1 (apair 2 (apair 3 (apair 4 (apair 5 (munit))))))
                 "racketlist->mupllist test")

   (check-equal? (mupllist->racketlist (apair 8 (apair 6 (apair 4 (apair 2 (apair 0 (munit)))))))
                 (list 8 6 4 2 0)
                 "mupllist->racketlist test")
   
   (check-equal? (eval-exp
                  (mlet "x" (add (int 2) (int 3))
                        (ifnz (isgreater (var "x") (int 0))
                              (add (int 4) (add (int 1) (var "x")))
                              (add (int 2) (var "x")))))
                 (int 10)
                 "mlet, ifnz, & isgrader expression test")

   (check-equal? (eval-exp (ifnz (ismunit (first (apair (munit) (int 7))))
                                 (int 1)
                                 (int -1)))
                 (int 1)
                 "ismunit, first, apair, ifnz true test")

   (check-equal? (eval-exp (ifnz (ismunit (second (apair (munit) (int 7))))
                                 (int 1)
                                 (int -1)))
                 (int -1)
                 "ismunit, second, apair, ifnz true test")
   
   (check-equal? (eval-exp (mlet "doubleFunc" (fun null "x" (add (var "x") (var "x")))
                  (call "doubleFunc" (int 2))))
                 (int 4)
                 "function call test")

   (check-equal? (ifmunit (munit) (int 1) (int 0))
                 (int 1)
                 "ifmunit true test")
   
   (check-equal? (ifmunit (int 2) (int 1) (int 0))
                 (int 0)
                 "ifmunit false test")

   (check-equal? (ifeq (int 3) (int 3) (int 1) (int 0))
                 (int 1)
                 "ifeq true test")
   
   (check-equal? (ifeq (int 2) (int 4) (int 1) (int 0))
                 (int 0)
                 "ifeq false test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
