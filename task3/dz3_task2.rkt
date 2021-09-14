#lang scheme
(define (process lst)
  (if (null? lst)
      '()
      (let ((sum (foldl (lambda (x y) (* x y)) 1 (car lst))))
        (filter (lambda (z) (> (foldl (lambda (x y) (+ x y)) 0 z) sum)) lst)
        )
      )
  )

(process '((5 2) (1 2) (1 2 3 4 5 6 7) (3 4 8) (2 3) (2 3 4)))