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

(define my-reverse
  (lambda (lst)
    ((Y (lambda (f)
          (lambda (lst acc)
            (if (equal? lst '())
                acc
                (let ([f (force f)])
                  (f (my-cdr lst)
                     (my-cons (my-car lst)
                              acc)))))))
     lst '())))

(define my-map
  (lambda (thunk lst)
    ((Y (lambda (f)
          (lambda (thunk lst acc)
            (if (equal? lst '())
                (my-reverse acc)
                (let ([f (force f)])
                  (f thunk
                     (my-cdr lst)
                     (my-cons (thunk (my-car lst))
                              acc)))))))
     thunk lst '())))

(define my-range
  (lambda (start end)
    ((Y (lambda (f)
          (lambda (start end acc)
            (if (< start end)
                (let ([f (force f)])
                  (f (+ start 1) end
                     (my-cons start
                              acc)))
                (my-reverse acc)))))
     start end '())))

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
