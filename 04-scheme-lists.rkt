#lang racket

;;task 1
(define (sort-list l)
  (lambda (f) (sort l f))
  )

;;task 2
(define (my-length l)
  (length l)
  )

;;task 3
(define (get-smallest l)
  (first (sort l <))
  )

;;task 4
(define (remove-first element list)
  (remq element list))

(define (remove-all x xs)
  (cond
    [(empty? xs) xs]
    [(equal? (car xs) x) (remove-all x (cdr xs))]
    [else (cons (car xs) (remove-all x (cdr xs)))]
    )
  )

;;task 5
(define (num-to-xs number)
  (if (< number 10)
      (remainder number 10)
      (flatten (cons (num-to-xs (quotient number 10)) (remainder number 10)))
      )
  )

(define (xs-to-num list)
 (define (helper number list  decimal)
    (if (null? list)
      number
      (helper (+ number (* (expt 10 decimal) (car list))) (cdr list) (- decimal 1))
      )
    )
  (helper 0 list (- (length list) 1))
  )

;;task 6
(define (set-union list-one list-two)
  (sort (remove-duplicates (append list-one list-two)) <)
  )

;;task 7
(define (my-reverse-foldl list)
  (foldl (lambda (x y) (cons x y)) '() list)
  )

;;task 8
(define (kth-max-negative list)
  (define (helper list)
    (if (< (car list) 0)
        list
        (helper (cdr list))
        )
    )
  (lambda (x) (list-ref (helper (sort list >)) (- x 1)))
  )

;;task 9
(define (insert-at x position xs)
  (cond
    [(zero? position) (cons x xs)]
    [(empty? xs) (list x)]
    [else (cons (car xs) (insert-at x (sub1 position) (cdr xs)))]
    )
  )
;;task 10
(define (concat xs-1 xs-2)
  (append xs-1 xs-2)
  )

;;task 11
(define (factorise number)
  (define (helper number divisor xs)
    (if (= divisor number)
        (flatten (list xs divisor))
        (if (zero? (remainder number divisor))
            (helper (quotient number divisor) divisor (list divisor))
            (helper number (+ divisor 1) xs)
            )
        )
    )
  (helper number 2 '())
  )

;;task 12
  