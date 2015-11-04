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
  (lambda (f)
    (lambda (thunk lst)
      (if (equal? lst '())
          '()
          (my-cons (thunk (my-car lst))
                   ((force f) thunk (my-cdr lst)))))))

(define my-range
  (lambda (f)
    (lambda (start end)
      (if (< start end)
          (my-cons start
                   ((force f) (+ start 1) end))
          '()))))

(define fibonacci
  (lambda (f)
    (lambda (n)
      (if (<= n 1)
          1
          (+ ((force f) (- n 1))
             ((force f) (- n 2)))))))

((Y my-map) (lambda (x) (write x) (newline))
            ((Y my-map) (Y fibonacci)
                        ((Y my-range) 0 10)))
