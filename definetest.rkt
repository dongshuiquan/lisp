#lang racket
(define (square x)(* x x))

(define (sum-of-square x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-square( + a 1) (* a 2)))

(define (abs x)
  (cond ((> x 0) x)
        (else ( - x))
))

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(define (average x y)
  (/ (+ x y) 2))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial(- n 1)))))

(define (fact n)
  (define (iter product counter)
    (if ( > counter n)
        product
        (iter(* counter product) (+ counter 1))
        ))
  (iter 1 1))

(define (fib n)
  (define (fib-iter a b n)
    (if (= n 0)
        b
        (fib-iter (+ a b) a (- n 1) )
        )
    
    )
  (fib-iter 1 0 n))