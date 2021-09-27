#lang scheme
;ЗАДАЧА 1

;в вершине списка лежит текущая позиция
;второй элемент это минимальный элемент
;далее идет список из номеров
(define (taskI lst)
  (cond [ (null? lst) lst ]
        [ (= 1 (length lst)) '(0) ]
        [else (cddr (foldl (lambda (x y)
                       (let ( (cur-pos (car y)) (min (cadr y)) )
                         (cond [ (= x min) (cons (add1 cur-pos) (cons min (cons cur-pos (cddr y))))]
                               [ (< x min) (list (add1 cur-pos) x cur-pos) ]
                               [else (cons (add1 cur-pos) (cdr y))])
                          )) (list 1 (car lst) 0) (cdr lst)))]
        )
  )

(taskI (list 0 1 1 -2 -3 -1 -3 0 1))
(taskI (list -1 0 1 -1 0 1 -1))

;ЗАДАЧА 2
(define (taskII t s)
  (let loop ( (t t) (s s))
    (if (not (vector? t)) (* t s)
        (+ (loop (vector-ref t 0) (/ s 4.0))
           (loop (vector-ref t 1) (/ s 4.0))
           (loop (vector-ref t 2) (/ s 4.0))
           (loop (vector-ref t 3) (/ s 4.0)))
        )
    )
  )

(newline)
(taskII #(1 0 0 #(1 1 1 0)) 16)
(newline)
           

;ЗАДАЧА 3

; создается список длин (чтобы не пересчитывать их при втором проходе по списку)
; и попутно вычисляется самая большая длина
; затем применяется функция map
(define (taskIII lst)
  (if (null? lst) '()
      (let* ( (list-of-length (foldl (lambda (x y) (let ( (len (foldl (lambda (x1 y1) (add1 y1)) 0 x)) (max-len (car y)) )
                                                     (if (> len max-len) (cons len (cons len (cdr y)))
                                                         (cons max-len (cons len (cdr y)))))) '(0) lst)) (max-len (car list-of-length)))
        (map (lambda (x y) (if (= y max-len) (map add1 x) x)) lst (foldl (lambda (x y) (cons x y)) '() (cdr list-of-length))) )
      )
  )

(taskIII '((1) (2 3) (4 5) (6)))

;ЗАДАЧА 4
(define (taskIV t s)
  (let loop ( (t t) (s s) (cc (lambda (x) x)))
    (if (not (vector? t)) (cc (* t s))
        (loop (vector-ref t 0) (/ s 4.0) (lambda (y)
        (loop (vector-ref t 1) (/ s 4.0) (lambda (z)
        (loop (vector-ref t 2) (/ s 4.0) (lambda (w)
        (loop (vector-ref t 3) (/ s 4.0) (lambda (u)
           (cc (+ y z w u))))))))))
        )
    )
  )

(taskIV #(1 0 0 #(1 1 1 0)) 16)
(newline)
          

; ЗАДАЧА 5
(define (taskV func1 . tail)
  (foldl (lambda (x y) (lambda (z) (y (x z)))) func1 tail)
  )

( (taskV (lambda (x) (* x x)) (lambda (x) (* x 2)) add1) 1)

