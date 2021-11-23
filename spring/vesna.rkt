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
                   
(define sentence (get-sentence "Republic.txt" (list "." "!" "?") ))

; обрезает список
(define (trunc-list lst size)
    (let loop ( (lst lst) (res '()) (size size))
      (if (= size 0) (reverse res)
          (loop (cdr lst) (cons (car lst) res) (sub1 size))
          )
      )
    )

;заполняет таблицу N-1 грамм в прямом порядке
;сделать начала предложений в нижнем регистре перед loop
;string-downcase
(define (fill-graph sentence-generator)    
  
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


; возвращает номер выбранной стратегии
; func - доступ к весу
(define (choose-list-elem weight-sum func lst)
  (let ( (result (random weight-sum)) )
   (let loop ( (result result) (lst lst) (i 0))
      (let ( (new-res (- result (func (car lst)))) )
        (if (< new-res 0) (car lst) (loop new-res (cdr lst) (add1 i)))
        )
      )
    )
  )

; выбор продолжения N-1 граммы
; сильнее хочет закончить предложения в зависимости от длины
(define (choose-continue N-1-gram graph len-sentence)
   (let ( (res-in-graph (hash-ref graph N-1-gram #f)) )
     (if (> len-sentence (* N 3))
         (let ( (dif (quotient (- len-sentence (+ N N)) N)) )
           (car (choose-list-elem (+ (car res-in-graph) dif) cdr (append (hash->list (cdr res-in-graph))
                                                                                            (list (cons "." dif))))))
         (car (choose-list-elem (car res-in-graph) cdr (hash->list (cdr res-in-graph))))
     )
     )
  )


; функция, которой на вход подается начало предложения, а она дальше уже строит до конца
(define (direct-gen-with-beginning graph N-begin separators-end)
  (let loop ( (res N-begin) (N-1-gram (cdr N-begin)) (len-sentence N) )
    (let ( (word (choose-continue N-1-gram graph len-sentence)) )
      (if (ormap (lambda (x) (equal? x word)) separators-end) (append res (list word))
          (loop (append res (list word)) (append (cdr N-1-gram) (list word)) (add1 len-sentence))
          )
      )
    )
  )
      

; генерация ответной реплики в прямом порядке
(define (direct-generation graph-start graph-direct)
  
  (define (weight-sum graph)
    (let loop ( (graph-as-list (hash->list graph)) (res 0) )
      (if (null? graph-as-list) res
          (loop (cdr graph-as-list) (+ res (cadr (car graph-as-list))))
          )
      )
    )
  
  (let ( (weight-graph-start (weight-sum graph-start))   (size-graph-start (hash-count graph-start))   (list-graph-start (hash->list graph-start))
         (weight-graph-direct (weight-sum graph-direct)) (size-graph-direct (hash-count graph-direct)) (list-graph-direct (hash->list graph-direct)))
      (let* ( (start-N-1-gram (choose-list-elem weight-graph-start (lambda (x) (cadr x)) list-graph-start))
              (word (choose-list-elem (cadr start-N-1-gram) cdr (hash->list (cddr start-N-1-gram))))
              (result-start (append (car start-N-1-gram) (list (car word)))) )
        (direct-gen-with-beginning graph-direct result-start (list "." "!" "?" ".’"))
        )
    )
  )

; генерация ответной реплики в обратном порядке
; функция, которой на вход подается часть предложения (N-1 gram), а она дальше уже строит до начала
(define (backward-gen-with-middle graph N-1-middle separators-end)
  (let loop ( (res N-1-middle) (N-1-gram N-1-middle) (len-sentence (sub1 N)) )
    (let ( (word (choose-continue N-1-gram graph len-sentence)) )
      (if (ormap (lambda (x) (equal? x word)) separators-end) res
          (loop (cons word res) (cons word (trunc-list N-1-gram (sub1 (length N-1-gram)))) (add1 len-sentence))
          )
      )
    )
  )

; генерация ответной реплики в прямом порядке
; функция, которой на вход подается часть предложения (N-1 gram), а она дальше уже строит до конца
(define (direct-gen-with-middle graph N-1-middle separators-end)
  (let ( (word (choose-continue N-1-middle graph (sub1 N))) )
      (if (ormap (lambda (x) (equal? x word)) separators-end) (append N-1-middle (list word))
          (direct-gen-with-beginning graph (append N-1-middle (list word)) separators-end)
      )
    )
  )

; создает список всех возможных подсписков списка определенной длины
(define (create-sublists-list lst size)
  (let loop ( (lst lst) (res '()) (len (length lst)) )
    (if (< len size) res
        (loop (cdr lst) (cons (trunc-list lst size) res) (sub1 len))
        )
    )
  )



; поиск в хэш таблице похожих N-1 грамм
(define (hash-ref-custom graph N-1-quote)
  (let ( (hash-keys-lst (hash-keys graph)) )
  (if (hash-has-key? graph N-1-quote) N-1-quote
      (let loop1 ( (size (- N 2)) )
        (cond ( (= size 0) #f)
              ( (= size 1)
                (let ( (length-order-lst (sort (create-sublists-list N-1-quote size) (lambda (x y) (> (string-length x) (string-length y))) #:key car)) )
                  (let loop3 ( (length-order-lst length-order-lst) (keys-lst hash-keys-lst) )
                    (if (null? length-order-lst)
                        (void)
                        (if (null? keys-lst)
                            (loop3 (cdr length-order-lst) hash-keys-lst)
                            (if (ormap (lambda (x) (equal? (caar length-order-lst) (car x))) (create-sublists-list (car keys-lst) size))
                                (car keys-lst)
                                (loop3 length-order-lst (cdr keys-lst))
                                )
                            )
                        )
                    )
                  )
                )
              (else 
               (let loop2 ( (keys-lst (hash-keys graph)) (sublists-list (create-sublists-list N-1-quote size)) )
                 (if (null? keys-lst) (loop1 (sub1 size))
                     (if (ormap (lambda (x) (ormap (lambda (y) (equal? x y)) (create-sublists-list (car keys-lst) size))) sublists-list)
                         (car keys-lst)
                         (loop2 (cdr keys-lst) sublists-list)
                         )
                     )
                 )
               )
              )

            )
        )
      )
  )           


;генерация ответной реплики в смешанном режиме
(define (mixed-generation graph-direct graph-backward N-1-quote)
    (append (backward-gen-with-middle graph-backward (hash-ref-custom graph-direct N-1-quote) (list "." "!" "?" "’"))
           (list-tail (direct-gen-with-middle graph-direct (hash-ref-custom graph-direct N-1-quote) (list "." "!" "?" "’")) (sub1 N)))
  )
  

  
(fill-graph sentence)
(display "END OF FILL GRAPHS\n")
;(direct-generation graph-start graph-direct)

;(create-sublists-list (car (hash-keys graph-direct)) 1)
;(create-sublists-list (cadr (hash-keys graph-direct)) 1)
;(create-sublists-list (caddr (hash-keys graph-direct)) 1)
;(test (list "dasdas" "asd" "eqweqweqweqwqw" "me" "absolute" "asda") 1 (hash-keys graph-direct))
(mixed-generation graph-direct graph-backward (list "dasdas" "asd" "of" "the" "absolute" "truth"))
;graph-start

;graph-start
;(define result (list graph-start graph-direct graph-backward))
;(write-to-file result "save.bin"  #:exists 'truncate)
;(define result-list (file->value "save.bin"))
;(cadr result-list)

; содержит сам граф, а также общую
;(hash->list graph-start)
  




  


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

          