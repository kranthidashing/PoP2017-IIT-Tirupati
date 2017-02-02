#lang racket

(define (qsort a)
  (if (empty? a)
    a
    (let ([p (car a)])
      (let ([tail (cdr a)])
        (let ([lsr (filter (lambda (x) (< x p)) tail)])
          (let ([grt (filter (lambda (x) (>= x p)) tail)])
            (append (qsort lsr) (list p) (qsort grt))))))))
