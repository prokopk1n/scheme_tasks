#lang scheme
(require racket/vector)

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))


(define (print-tree-by-level-desc tree)

  (define (list-level-tree-cps tree level cc)
    (cond ( (empty-tree? tree) (cc '()) )   
          ( (= level 1) (cc (list (tree-data tree))) )
          (else (list-level-tree-cps (tree-right tree) (sub1 level) (lambda (y) (list-level-tree-cps (tree-left tree) (sub1 level) (lambda (z) (cc (append y z)))))) )
          )
    )

  (define (depth-tree-cps tree cc)
    (if (empty-tree? tree) (cc 0)
        (depth-tree-cps (tree-right tree) (lambda (y) (depth-tree-cps (tree-left tree) (lambda (z) (cc (add1 (max y z)))))))
        )
    )

  
  
  (let loop ( (n (depth-tree-cps tree (lambda (x) x))) )
    (if (= n 0) (void)
        (begin
          (display (string-join (map number->string (list-level-tree-cps tree n (lambda (x) x))) " "))
          (newline)
          (loop (sub1 n)))
        )
    )
  )


(print-tree-by-level-desc #(10 #(21 #(31 #() #()) #()) #(22 #(33 #() #()) #(34 #() #()))))
(newline)
(print-tree-by-level-desc #())
(newline)
(print-tree-by-level-desc #(1 #() #()))
(newline)
(print-tree-by-level-desc #(10 #(21 #() #()) #(22 #() #())))





        