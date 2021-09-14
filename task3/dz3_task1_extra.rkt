#lang scheme

(define (enumerate-interval n)
(let loop ((i n) (result '()))
  (if (= i 0) result
      (loop (- i 1) (cons i result))
      )
  )
  )

(enumerate-interval 5)

(define (list-fib-squares-a n)
  (cond ((or (not (integer? n)) (<= n 0)) '())
        ((= n 1) '(1))
        ((= n 2) '(1 1))
        (else (cdr (foldl (lambda (x y) (if (or (null? y) (not (= (car y) 0))) (cons 0 (cons x y)) (cdr y))) '(0)
        (foldl (lambda (x y) (let ((fib-x (+ (car y) (car (cdr (cdr y))))))
                                 (cons fib-x (cons (* fib-x fib-x) y)))) '(1 1 1 1) (enumerate-interval (- n 2))))))
        )
        
  )

(list-fib-squares-a 5)