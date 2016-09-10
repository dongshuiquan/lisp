#lang racket
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1)(* a 2)))

(define (abs x)
  (cond ((> x  0) x)
        ((= x  0) 0)
        ((< x  0) (- x))))

(define (abs2 x)
  (if (< x 0)
      (- x)
      x))

(define (a-plus-abs-b a b)
  ((if ( > b 0) + -) a b))

(define (inc n)(+ n 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum-cubes2 a b)
  (sum cube a inc b))

(define plus4 (lambda (x) (+ x 4)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter(improve guess x)
                x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ ( + x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt2 x)
  (define (good-enough? guess x)
    (< (abs ( - (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter(improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (factorial n)
  (if(= n 1)
  1
  (* n (factorial (- n 1)))))

(define (fact n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* product counter)
              (+ counter 1))))
  (iter 1 1))
    
(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else(+ (fib(- n 1)) (fib(- n 2))))))

(define(fib2 n)
  (fib-iter 1 0 n))
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  
 
  (define (count-change amount)
    (cc amount 5))
  
  (define (cc amount kinds-of-coins)
    (cond (( = amount 0) 1)
          ((or (< amount 0) ( = kinds-of-coins 0)) 0)
          (else ( + (cc amount
                        (- kinds-of-coins 1))
                    (cc (- amount
                           (first-denomination kinds-of-coins))
                        kinds-of-coins)))))
  
  (define (first-denomination kinds-of-coins)
    (cond(( = kinds-of-coins 1) 1)
         (( = kinds-of-coins 2) 5)
         (( = kinds-of-coins 3) 10)
         (( = kinds-of-coins 4) 25)
         (( = kinds-of-coins 5) 50)))
  
  (define ( expt b n)
    (if ( = n 0)
        1
        (* b (expt b (- n 1)))))
  
  (define (expt2 b n)
    
    (define (expt-iter b counter product)
      (if(= counter 0)
         product
         (expt-iter b (- counter 1) (* product b))))
    (expt-iter b n 1))
  
  (define (enev? n)
    ( = (remainder n 2) 0))
  
  (define (fast-expt b n)
    (cond((= n 0) 1)
         ((even? n)(square (fast-expt b (/ n 2))))
         (else ( * b (fast-expt b (- n 1))))))
  
  
  (define (smallest-divisor n)
    (find-divisor n 2))
  
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n ) n)
           ((divides? test-divisor n) test-divisor)
           (else ( find-divisor n ( + test-divisor 1)))))
  
  (define (divides? a b)
    (= (remainder b a) 0))
  
  (define (prime? n)
    (= n (smallest-divisor n)))
  
  
  (define ( expmod base exp m)
    (cond (( = exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m))
                      m))
          (else
           (remainder (* base (expmod base ( - exp 1) m))
                      m))))
  
  
  (define (sum-intergers a b)
    (if ( > a b)
        0
        (+ a (sum-intergers (+ a 1) b))))
  
  (define (cube x)(* x x x))
  
  
  (define (sum-cubes a b)
    (if (> a b)
        0
        (+ (cube a) (sum-cubes ( + a 1) b))))
  
  (define (pi-sum a b)
    (if(> a b)
       0
       (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
  
  
  (define (fxy x y)
    (let ((a (+ 1 {* x y}))
          (b (- 1 y)))
      (+ (* x (square a))
         (* y b )
         (* a b))))
  
  
  (define (sqrt3 x)
    (fix
  
  (define (average-damp f)
    (lambda (x) (average x (f x))))
