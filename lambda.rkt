#lang racket

(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (g)
       (f (delay (g g)))))))

(define my-cons
  (lambda (car cdr)
    (lambda (selector)
      (selector car cdr))))

(define my-car
  (lambda (cons)
    (cons (lambda (car cdr)
            car))))
(define my-cdr
  (lambda (cons)
    (cons (lambda (car cdr)
            cdr))))

(define my-map
  (Y (lambda (f)
       (lambda (thunk lst)
         (if (equal? lst '())
             '()
             (my-cons (thunk (my-car lst))
                      ((force f) thunk (my-cdr lst))))))))

(define my-range
  (Y (lambda (f)
       (lambda (start end)
         (if (< start end)
             (my-cons start
                      ((force f) (+ start 1) end))
             '())))))

(define fibonacci
  (Y (lambda (f)
       (lambda (n)
         (if (<= n 1)
             1
             (+ ((force f) (- n 1))
                ((force f) (- n 2))))))))

(void
 (my-map (lambda (x) (write x) (newline))
         (my-map fibonacci
                 (my-range 0 10))))
