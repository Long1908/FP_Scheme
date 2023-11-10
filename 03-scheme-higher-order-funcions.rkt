)#lang r5rs
;; task 1
(define (my-identity)
  (lambda (x) x)
  )

(define (my-lambda f)
  (lambda (x) (f x))
  )

(define (negate-pred p)
  (lambda (x) (not(p x)))
  )

(define (my-compose f g)
  (lambda (x) (f (g x)))
  )
;;task 2
(define (complex-procedure f g)
  (lambda (x y) (f (g x y)))
  )
;;task 3
(define (apply-n f n)
  (define (helper i result)
    (if (> i n)
        result
        (helper (+ i 1) (f result)))
    )
  (lambda (x) (helper 1 x))
  )