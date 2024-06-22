#lang racket
(require rebellion/collection/list)

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(define (f n) (A 0 n))

(define (f-rec n)
  (if (< n 3)
      n
      (+
       (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3)))
      )
  )
)

(define (log name value)
  (printf "~a: ~a ~n" name value))

(define (f-iter n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (else
         (f-iter-inner 2 1 0 n))))

(define (f-iter-inner f_n f_n_1 f_n_2 n)
  (if (= n 3)
      (+ (* 3 f_n_2) (* 2 f_n_1) f_n)
      (f-iter-inner (+ (* 3 f_n_2) (* 2 f_n_1) f_n) f_n f_n_1 (- n 1))))

(define (list-generator list-size a-list)
  (lambda (n)
    (cond ((= n 0) 1)
          ((= n (- list-size 1)) 1)
          (else (+  (list-ref a-list n) (list-ref a-list (- n 1)))))))

(define (create-next previous )
  (define length (list-size previous))
  (build-list (+ length 1) (list-generator (+ length 1) previous)))

(define (pascal-rec level)
  (cond ((= level 0) (values
                      (create-next (list))
                      (list)))
        (else
         (define-values (to-create return-list) (pascal-rec (- level 1)))
         (values (create-next to-create) (cons return-list to-create)))))


(define (even? n)
  (= (remainder n 2) 0))

(define (squared n)
  (* n n))

(define (fast-expt a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt a (squared b) (/ n 2)))
        (else (fast-expt (* a b) (squared b) (/ (- n 1) 2)))))
