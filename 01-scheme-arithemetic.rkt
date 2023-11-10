#lang r5rs

(define (succ n) (+ n 1))

(define (loop-for-pred n i)
  (if (= (succ i) n)
      i
      (loop-for-pred n (succ i))))

(define (pred n)
  (loop-for-pred n 0))

(define (add a b)
  (if (= b 0)
      a
      (add (succ a) (pred b))))

(define (loop-for-mul a a-copy b i)
  (if (= i b)
      a
      (loop-for-mul (add a a-copy) a-copy b (succ i))))


(define (mul a b)
  (loop-for-mul a a b 1))
      
(define (fact n)
  (if (= n 1)
      1
      (mul n (fact (pred n)))))

(define (safe-div-loop n i)
  (if (> n (mul i 2))
      (safe-div-loop n (mul i 2))
      (cond
        ((= n (mul i 2)) i)
        ((< n (mul i 2)) (pred i)))))
      
(define (safe-div n)
  (safe-div-loop n 1))
  