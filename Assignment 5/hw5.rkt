;; CSE341, Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist rList)
  (if (null? rList)
      (munit)
      (apair (car rList) (racketlist->mupllist (cdr rList)))))

(define (mupllist->racketlist mList)
  (if (ismunit mList)
      null
      (cons (first mList) (mupllist->racketlist (second mList)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(munit? e) e]
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1)
                      (int-num v2))
                   (int 1)
                   (int 0))
               (error "MUPL isgreater applied to non-number")))]
        [(ifnz? e)
         (let ([v (eval-under-env (ifnz-e1 e) env)])
           (if (int? v)
               (if (= (int-num v) 0)
                   (eval-under-env (ifnz-e3 e) env)   ; ifnz body allows non-int expressions
                   (eval-under-env (ifnz-e2 e) env))  ; ifnz body allows non-int expressions
               (error "MUPL ifnz condition applied to non-number")))]
        [(fun? e)
         (let* ([name (fun-nameopt e)]
                [param (fun-formal e)]
                [newEnv (cons (cons name (closure env e)) env)])  ; fun body allows non-int expressions
           (if (string? param)
               (cond [(string? name) (closure newEnv e)]      ; regular function
                     [(null? name) (closure env e)]           ; anonymous function
                     [#t (error "MUPL function name binding must be a string or null")])
               (error "MUPL function param binding applied to non-string")))]
        [(mlet? e)
         (let* ([name (mlet-var e)]
                [valu (eval-under-env (mlet-e e) env)] ; letm binding value allows non-int expressions
                [newEnv (cons (cons name valu) env)])
           (if (string? name)
               (eval-under-env (mlet-body e) newEnv)   ; letm body allows non-int expressions
               (error "MUPL mlet binding applied to non-string")))]
        [(call? e)
         (let ([funcl (envlookup env (call-funexp e))]
               [arg (call-actual e)])
           (if (closure? funcl)
               (let* ([lexEnv (closure-env funcl)]
                      [fn (closure-fun funcl)]
                      [param (fun-formal fn)]
                      [newEnv (cons (cons param arg) lexEnv)])
                 (eval-under-env (fun-body fn) newEnv))
               (error (string-append "MUPL call '" (call-funexp e) "' is not a function"))))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(first? e) (if (pair? e)
                        (eval-under-env (apair-e1 (first-e e)) env)
                        (error "MUPL first applied to non-pair"))]
        [(second? e) (if (pair? e)
                         (eval-under-env (apair-e2 (second-e e)) env)
                         (error "MUPL second applied to non-pair"))]
        [(ismunit? e) (if (munit? (eval-under-env (ismunit-e e) env)) (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-filter "CHANGE")

(define mupl-all-gt
  (mlet "filter" mupl-filter
        "CHANGE (notice filter is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
