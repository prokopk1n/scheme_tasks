#lang scheme
(define (odd-fib-list n)
  (define (loop i fib-n-1 fib-n-2)
    (let ((fib-n (+ fib-n-1 fib-n-2)))
      (if (= i 0)
          '()
          (if (odd? fib-n)
              (cons fib-n (loop (- i 1) fib-n fib-n-1))
              (loop i fib-n fib-n-1)
          )
       )
     )
   )
  (if (or (not (integer? n)) (<= n 0) ) '() (cons 1 (loop (- n 1) 1 0)))
)



(define (odd-fib-list-iter n)
  (if (not (integer? n))
      '()
      (let loop ((i n) (list-cur '()) (fib-n-1 1) (fib-n-2 0))
        (if (<= i 0)
            (reverse list-cur)
            (if (odd? fib-n-1)
                (loop (- i 1) (cons fib-n-1 list-cur) (+ fib-n-1 fib-n-2) fib-n-1)
                (loop i list-cur (+ fib-n-1 fib-n-2) fib-n-1)
            )
         )
       )
   )
)

  
