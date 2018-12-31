;; Programming Languages, Homework 5 version 1.1
#lang racket
;; CSC330 Fall2018 Assignment 5
;; Zhe(Kevin) Chen

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; WArning: Do not use any mutation (set!, set-mcar!, etc.) anywhere in the assignment.

;; copied from hw5tests for the purpose of debugging
(define-syntax-rule (handler s)
   (with-handlers ([(lambda (v) #t) (lambda (v) "this is an invalid value. probably due to an exception")]) s))
(require rackunit)

;; Problem A

;; CHANGE (put your solutions here)
; 2  MUPL list to Racket list
(define (mupllist->racketlist lst)
  (letrec ([f (lambda (ls)
                (cond [(aunit? ls) null] ; (isaunit ls)
                      [(var? (apair-e1 ls)) (cons (apair-e1 ls) (f (apair-e2 ls)))] ; (fst ls) (snd ls)
                      [(int? (apair-e1 ls)) (cons (apair-e1 ls) (f (apair-e2 ls)))]
                      [(apair? (apair-e1 ls)) (cons (f (apair-e1 ls)) (f (apair-e2 ls)))]
                      [#t (f (apair-e2 ls))]))]) ; if non above simply skip this one
    (f lst)))

; 1  Racket list to analogous MUPL list
(define (racketlist->mupllist lst)
 (letrec ([f (lambda (ls)
                (cond [(null? ls) (aunit)]
                      [(or (var? (car ls)) (int? (car ls))) (apair (car ls) (f (cdr ls)))]
                      [(list? (car ls)) (apair (f (car ls)) (f (cdr ls)))]
                      [#t (f (cdr ls))]))]) ; if non above simply skip this one
    (f lst)))

;; Problem B

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

; 3
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
        ;; "CHANGE" add more cases here
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2)) ; compare 
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater e1 and(or) e2 is(are) non-number")))] ;  ifgreater (e1 e2 e3 e4)
        [(fun? e) (closure env e)] ; fun  (nameopt formal body) a recursive(?) 1-argument function
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]  ; funexp
               [v2 (eval-under-env (call-actual e) env)]) ; actual
           (if (closure? v1)
               (let* ([f1 (closure-fun v1)]
                      [e1 (closure-env v1)]
                      [cn (cons (fun-nameopt f1) v1)]
                      [cf (cons (fun-formal f1) v2)])
                 (eval-under-env (fun-body f1)
                                 (if (not (car cn)) ; if not false #f
                                     (cons cf e1)
                                     (cons cf (cons cn e1)))))
               (error "MUPL call function applied to non-closure")))] ; call (funexp actual)
        [(mlet? e) ; mlet (var e body)
         (let ([v1 (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v1) env)))] ; (let var = e in body)
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (if (or
                (and (int? v1) (int? v2))
                (and (int? v1) (var? v2))
                (and (var? v1) (int? v2))
                (and (var? v1) (var? v2))
                (and (int? v1) (aunit))
                (and (var? v1) (aunit)))
               (apair v1 v2)
               (error "MUPL apair applied to non-MUPL expression")))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e) ; same as fst with return value apair-e2
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd applied to non-pair")))]
        [(aunit? e) e] ; not sure
        [(isaunit? e) ; evaluate to 1 if e is unit else 0
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? v1)
               (int 1)
               (int 0)))]
        ;; one for each type of expression
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

; 4
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3)) ; once e1 is aunit,(int 1) > (int 0)

; 5
(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2] ; base case, en to the body
        [else (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))])) ; s to be the variable bond

; 6 implement ifeq using isgreater
(define (ifeq e1 e2 e3 e4)
  (ifgreater e1 e2 e4 (ifgreater e2 e1 e4 e3))) ; e1>e2 then e4, else means e1<=e2, if e2>e1 then e4, else e2<=e1, means e2=e1, e3

;; Problem D

; 7
(define mupl-map
  (fun #f "f" ; anonymous function
       (fun "mupl-map" "lst" ; inner loop
            (ifgreater (isaunit (var "lst")) (int 0)
                       (aunit) ; base case, empty list
                       (apair
                        (call (var "f") (fst (var "lst")))
                        (call (var "mupl-map") (snd (var "lst"))))))))
;; this binding is a bit tricky. it must return a function.
;; the first two lines should be something like this:
;;

;;   (fun "mupl-map" "f"    ;; it is  function "mupl-map" that takes a function f
;;       (fun #f "lst"      ;; and it returns an anonymous function
;;          ...

;;
;; also remember that we can only call functions with one parameter, but
;; because they are curried instead of
;;    (call funexp1 funexp2 exp3)
;; we do
;;    (call (call funexp1 funexp2) exp3)
;; 

; 8
(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "i" ; anonymous function takes in i
             (call (var "map") ; recursive call
                   (fun #f "x" (add (var "x") (var "i"))))))) ; anonymous function add each x to i


(define (create_stream fs init)
  (letrec ([stream (lambda (count)
                     (cons (fs count) (lambda () (stream (+ count 1)))))])
    (lambda () (stream init))))