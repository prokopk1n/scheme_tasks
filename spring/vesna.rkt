#lang scheme


(define separators (list #\. #\! #\? #\, #\- #\; #\" #\\ #\' #\( #\) #\; #\—))

; Хэш-таблица N-1 грамма -> пара (вес, хэш таблица (слово, вес))

; чтобы предложения начинались со смыслом
(define graph-start (make-hash '()))

(define graph-direct (make-hash '()))
(define graph-backward (make-hash '()))

(define N 7)

;разбивает строку по пробелам
(define (split-string s (separators separators) (sep #\space))
  (let loop ( (string-list (string->list s)) (buf '()) (result '()) )
    (if (null? string-list) (reverse (if (not (null? buf)) (cons (list->string (reverse buf)) result) result))
        (let ( (elem (car string-list)) )
          (cond ( (equal? elem sep) (loop (cdr string-list) '() (if (not (null? buf)) (cons (list->string (reverse buf)) result) result)))
                ( (ormap (lambda (x) (equal? x elem)) separators) (loop (cdr string-list) '() (cons (string elem) (if (not (null? buf)) (cons (list->string (reverse buf)) result) result))))
                (else (loop (cdr string-list) (cons elem buf) result))
                )
          )
        )
    )
  )


(equal? "!" "!")
(split-string "The Republic of Plato is the, longest of his works with the exception of. Hello after all" separators)
(car (string->list "."))
(string #\.)

;что-то вроде генератора, сохраняет состояние, возвращает предложение как список слов
;rest - список слов
(define (get-sentence file-name end-separators)
  (let ( (in (open-input-file file-name)) (rest '()) )
    (lambda () (let loop ()
                 (if (ormap (lambda (x) (ormap (lambda (y) (equal? x y)) end-separators)) rest)
                   (let create-sentence ( (result '()) (buf rest))
                     (if (ormap (lambda (x) (equal? (car buf) x)) end-separators)
                         (begin
                           (set! rest (cdr buf))
                           (let ( (result (reverse (cons (car buf) result))) )
                             (cons (string-downcase (car result)) (cdr result))
                             )
                           )
                         (create-sentence (cons (car buf) result) (cdr buf))
                         )
                     )
                    (let ( (str (read-line in 'return-linefeed)) )
                      (if (eof-object? str) str (begin (set! rest (append rest (split-string str))) (loop)))
                      )
                    )
                 )
      )
    )
  )
                   
(define sentence (get-sentence "test.txt" (list "." "!" "?") ))

(define hash1 (make-hash '(("a" . 1)) ) )
(define hash2 hash1)
(hash-ref hash1 "a")
(hash-set! hash1 "a" 2)
(hash-ref hash1 "a")
(hash-ref hash2 "a")


;заполняет таблицу N-1 грамм в прямом порядке
;сделать начала предложений в нижнем регистре перед loop
;string-downcase
(define (fill-graph sentence-generator)    
  
  (define (trunc-list lst size)
    (let loop ( (lst lst) (res '()) (size size))
      (if (= size 0) (reverse res)
          (loop (cdr lst) (cons (car lst) res) (sub1 size))
          )
      )
    )
  
  (let loop ( (sentence (sentence-generator)) )
    (if (not (eof-object? sentence))
        (begin
        (if (>= (length sentence) N)
                   (let* ( (N-1-gram (trunc-list sentence (sub1 N))) (res (hash-ref graph-start N-1-gram #f)) (word (list-ref sentence (sub1 N))) )
                       (if res
                           (let ( (weight (hash-ref (cdr res) word #f)) )
                                 (begin (hash-set! graph-start N-1-gram (cons (add1 (car res)) (cdr res))) (if weight (hash-set! (cdr res) word (add1 weight))
                                     (hash-set! (cdr res) word 1)
                                     ))
                                 )
                           (hash-set! graph-start N-1-gram (cons 1 (make-hash (list (cons word 1)))))
                           )
                    )
                   (void)
            )
        (let loop1 ( (sentence sentence) )
             (if (>= (length sentence) N)
                   (let* ( (N-1-gram (trunc-list sentence (sub1 N))) (res (hash-ref graph-direct N-1-gram #f)) (word (list-ref sentence (sub1 N))) )
                     (begin
                       (if res
                           (let ( (weight (hash-ref (cdr res) word #f)) )
                                 (begin (hash-set! graph-direct N-1-gram (cons (add1 (car res)) (cdr res))) (if weight (hash-set! (cdr res) word (add1 weight))
                                     (hash-set! (cdr res) word 1)
                                     ))
                                 )
                           (hash-set! graph-direct N-1-gram (cons 1 (make-hash (list (cons word 1)))))
                           )
                       (loop1 (cdr sentence))
                      )
                    )
                   (void)
                )
             )
        (let loop2 ( (sentence (cdr (reverse (cons "." sentence)))) )
          (if (>= (length sentence) N)
             (let* ( (N-1-gram (reverse (trunc-list sentence (sub1 N)))) (res (hash-ref graph-backward N-1-gram #f)) (word (list-ref sentence (sub1 N))) )
                     (begin
                       (if res
                           (let ( (weight (hash-ref (cdr res) word #f)) )
                                 (begin (hash-set! graph-backward N-1-gram (cons (add1 (car res)) (cdr res))) (if weight (hash-set! (cdr res) word (add1 weight))
                                     (hash-set! (cdr res) word 1)
                                     ))
                                 )
                           (hash-set! graph-backward N-1-gram (cons 1 (make-hash (list (cons word 1)))))
                           )
                       (loop2 (cdr sentence))
                      )
                    )
                   (void)
               )
          )
        (loop (sentence-generator))
        )

        (void)
      )
    )
  )

(fill-graph sentence)
graph-start
(define result (list graph-start graph-direct graph-backward))
(write-to-file result "save.bin"  #:exists 'truncate)
(define result-list (file->value "save.bin"))
(cadr result-list)

;Когда генерируем в прямом порядке, то сначала выбираем из graph-start, а потом уже идем по graph-direct
;это делается, что избежать случаев, когда запятая стоит сразу или после первого слова
;Когда смешанный режим, то в обратную сторону по graph-backward, а вперед по graph-direct
#|
(fill-graph sentence)
(write-to-file graph-start "save.bin"  #:exists 'truncate)
graph-start
(sleep 10)
graph-direct
(sleep 10)
graph-backward
(sleep 10)
(define graph1 (file->value "save.bin"))
graph1
|#

          