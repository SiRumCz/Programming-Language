#lang racket
;; CSC330 Fall2018 Assignment 4
;; Zhe(Kevin) Chen

(provide (all-defined-out)) ;; so we can put tests in a second file

;; these definitions are simply for the purpose of being able to run the tests
;; you MUST replace them with your solutions
;;

; 1
(define (sequence low high stride)
  (if (< low (+ high 1))
      (cons low (sequence (+ low stride) high stride))
      null)) ; (+ high 1) means low <= high

; 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; 3
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs))))))) ; i is the remainder produced when dividing n by the list’s length

; 4 stream s and number n
(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s n)
                (let ([pr (s)])
                  (if (> 1 n)
                      null ; if 1 > n (n is less or equal to 0) then output null list, else '(head, f(tail, n-1))
                      (cons (car pr)(f (cdr pr) (- n 1))))))])
    (f s n)))

; 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (integer? (/ x 5)) ; (= 0 (modulo x 5)) should also works
                    (cons (- x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1)))) ; starts with 1

; 6
(define cat-then-dog
  (letrec ([f (lambda (name)
                (cond [(string=? "cat.jpg" name) (cons name (lambda () (f "dog.jpg")))]
                      [#t (cons name (lambda () (f "cat.jpg")))]))])
    (lambda () (f "cat.jpg")))) ; using string compare

; 7
(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (let ([pr (s)])
                  (lambda() (cons (cons 0 (car pr)) (f (cdr pr))))))]) ; returns a stream
    (f s)))

; 8 use (list-nth-mod list n)
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (let ([pr (cons (list-nth-mod xs n) (list-nth-mod ys n))])
                  (lambda() (cons pr (f (+ n 1))))))])
    (f 0)))

; 9 Use library functions vector-length, vector-ref, and equal?
(define (vector-assoc v vec)
  (letrec ([f (lambda (v vec pos) ; value, vector, current position of pointer
                (cond [(> 1 (- (vector-length vec) pos)) #f] ; when it reaches the end of vector, means no match found
                      [#t
                       (let ([cur (vector-ref vec pos)])
                         (cond [(equal? v (car cur)) cur] ; found match, return this match
                               [#t (f v vec (+ pos 1))]))]))]) ; go through next
    (f v vec 0)))

; 10 https://docs.racket-lang.org/reference/vectors.html
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n (list 0 #f))] ; The cache starts empty (all elements #f)
           [cslot 0] ; cache slots
           [f (lambda (x)
                (let ([result (vector-assoc x cache)]) ; check the cache
                  (if result
                      result ; return this result
                      (let ([ans (assoc x xs)]) ; use assoc and xs to get the answer
                            (if ans
                                (begin
                                  (vector-set! cache cslot ans) ; adds the pair to the cache
                                  (set! cslot (modulo (+ cslot 1) n)) ; increment the cacheslot
                                  ans)
                                ans)))))])
    f))

; 11 macro, your macro can do anything or fail mysteriously otherwise
(define-syntax while-less
  (syntax-rules (do) ; do is syntax(keywords)
    [(while-less e1 do e2)
     (let ([res1 e1]) ; save the result from e1, only eval once
       (letrec ([loop (lambda (res2)
                        (if (>= res2 res1) ; res2 from e2 is no less than res1
                            #t ; return the result
                            (begin (loop e2))))])
         (loop e2)))]))
