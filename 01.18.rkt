#lang racket

(define (even? n)
  (= (remainder n 2) 0))

(define (halve n)
  (if (even? n) (times-subtracted n 0) n))

(define (double n)
  (+ n n))

(define (times-subtracted n count)
  (if (= n 0) count (times-subtracted (- n 2) (+ count 1))))

(define (fast-multiply a b c)
  (cond  ((= a 0) c)
         ((even? a) (fast-multiply (halve a) (double b) c))
         (else (fast-multiply (- a 1) b (+ c b)))))
