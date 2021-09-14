#lang scheme
(define (list-fib-inverted n)
  (if (or (not (integer? n)) (<= n 0))
      '()
      (let loop ((i (- n 1)) (fib-n-1 1) (fib-n-2 0) (result '(1)))
          (if (= i 0)
              result
              (let ((fib-n (+ fib-n-1 fib-n-2)))
                (loop (- i 1) fib-n fib-n-1 (cons fib-n result))
              )
          )
        )
      )
  )


(define (list-fib-squares-a n)
  (foldl (lambda (x y) (cons x y)) '() (map (lambda (x) (* x x)) (list-fib-inverted n)))
)

(define (list-fib-squares-b n)
  (foldl (lambda (x y) (cons (* x x) y)) '() (list-fib-inverted n))
)

(list-fib-squares-a 5)
(list-fib-squares-b 5)
(list-fib-squares-a 7)
(list-fib-squares-b 7)
(list-fib-squares-a 8)
(list-fib-squares-b 8)

  

        
                       