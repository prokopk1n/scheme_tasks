; "Доктор". Осень 2021
#lang scheme
; В учебных целях используется базовая версия Scheme

(require racket/vector)
; подключаем функции для работы с векторами

(require racket/date)


; SPRING PART


(define separators (list #\. #\! #\? #\, #\- #\; #\" #\\ #\' #\( #\) #\; #\—))

; Хэш-таблица N-1 грамма -> пара (вес, хэш таблица (слово, вес))

; чтобы предложения начинались со смыслом
(define graph-start (make-hash '()))

(define graph-direct (make-hash '()))
(define graph-backward (make-hash '()))

(define N 7)

;разбивает строку по пробелам
;возвращает список слов/символов
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
  (let ( (N-1-quote-key (hash-ref-custom graph-direct N-1-quote)) )
    (append (backward-gen-with-middle graph-backward N-1-quote-key (list "." "!" "?" "’"))
           (list-tail (direct-gen-with-middle graph-direct N-1-quote-key (list "." "!" "?" "’")) (sub1 N)))
    )
  )
  
; TESTS
#|
(define sentence (get-sentence "Republic.txt" (list "." "!" "?") ))
(fill-graph sentence)
(display "END OF FILL GRAPHS\n")
(mixed-generation graph-direct graph-backward (list "dasdas" "asd" "of" "the" "absolute" "truth"))
(direct-generation graph-start graph-direct)
|#
; END OF SPRING PART

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name)
)

; part1
; доктор из первого блока, который поддерживает выбор новой фразы
; из предыдущих фраз пациента
(define (visit-doctor-2 name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v1 name #())
)

; part2
; запускает доктора для работы с amount клиентов или до того, пока кто-либо
; не назовет в качестве имени стоп-слов
; tail нужен для того, чтобы передавать предобработанную структуру
(define (visit-doctor-v2 stop-word amount . tail)
  (let ( (post-key-words (if (null? tail) (prepare keywords_structure) (car tail) ) ) ) 
   (if (> amount 0)
      (let ( (name (ask-patient-name) ) )
        (if (not (equal? name stop-word))
            (begin                                   
             (printf "Hello, ~a!\n" name)
             (print '(what seems to be the trouble?))
             (doctor-driver-loop-v2 name #() post-key-words)
             (visit-doctor-v2 stop-word (sub1 amount) post-key-words)
            )
            (void)
        )
      )
      (void)
     )
   )
)


(define (ask-patient-name)
 (begin
  (printf "next! ")
  (printf "who are you?\n- ")
  (car (split-string (read-line)))
 ) 
)

; part 2
; функция, которая обрабатывает структуру, которая позволяет
; строить овтетные реплики по ключевым словам пациента
; данная функция строит список ключевых слов
(define (prepare structure)
  (let loop ( (n (vector-length structure)) (res '()) )
    ( if (= n 0) res
         (loop (sub1 n) (foldl (lambda (x y) (if (not (member x y)) (cons x y) y)) res (car (vector-ref structure (sub1 n))))) )
    )
  )


; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)


; part1
; цикл с поддержкой истории ответов
(define (doctor-driver-loop-v1 name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else
                  (print (reply-v1 user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop-v2 name (vector-append history (vector user-response)))
             )
       )
      )
)


; part 2
; цикл с поддержкой как истории ответов, так и ответов по ключевым словам
(define (doctor-driver-loop-v2 name history post-key-words)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else
                  (print (reply-v2 user-response history post-key-words)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop-v2 name (vector-append history (vector user-response)) post-key-words)
             )
       )
      )
)


; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
      (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
      )
)

; part 1
; поддержка истории ответов
(define (reply-v1 user-response history)
      (case (if (vector-empty? history) (random 2) (random 3)) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
          ((2) (history-answer history)); 3й способ
        
      )
)

; part 2
; post-key-words - список из ключевых слов
; сама структура
(define (reply-v2 user-response history post-key-words)
      (let ( (min (if (find-key-words user-response post-key-words) 0 1)) (max (if (vector-empty? history) 3 4)))
        (case (random min max) ; с равной вероятностью выбирается один из четырех способов построения ответа
          ((0) (key-word-answer user-response post-key-words)) ; 1й способ - генерация ответа по ключевому слову
          ((1) (qualifier-answer user-response)) ; 2й способ - замена я на вы и тп
          ((2) (hedge))  ; 3й способ
          ((3) (history-answer history)); 4й способ
        
        )
      )
)

; part 2
; вспомогательная функция
; проверяет пересечение двух списков (есть ли одинаковые элементы)
(define (list-cross? lst1 lst2)
  (ormap (lambda (y) (member y lst2)) lst1)
  )

; part 2
; вспомогательная функция
; выбор случайного элемента из списка
(define (pick-random-list lst)
  (list-ref lst (random (length lst)))
  )

; part 2
; проверяет, есть ли ключевое слово в ответе
(define (find-key-words user-response post-key-words)
  (list-cross? user-response post-key-words)
  )


; part 2
; 1й способ - генерация ответа по ключевым словам
(define (key-word-answer user-response post-key-words)
  (select-reply-expression user-response post-key-words)
  )

; part 2
; выбор ответной реплики по ключевому слову
(define (select-reply-expression user-response post-key-words)
  (let* ( (key-word (select-key-word user-response post-key-words))
         (vector-expressions (vector-filter (lambda (x) (member key-word (car x))) keywords_structure) ) )
    (many-replace-v3 (list (list '* key-word)) (pick-random-list
                                                (foldl (lambda (x y) (append x y))
                                                         '() (vector->list (vector-map (lambda (x) (cadr x) ) vector-expressions)))))
    )
  )

; part 2
; выбор ключевого слова из реплики
(define (select-key-word user-response post-key-words)
  (pick-random-list (make-key-words-list user-response post-key-words))
  )
    

; part2
;формирует список из ключевых слов в реплике
(define (make-key-words-list user-response post-key-words)
  (foldl (lambda (x y) (if (member x post-key-words)
                           (cons x y)
                           y)) '() user-response)
  )


; part 2
; структура из ключевых слов и ответых реплик
(define keywords_structure '#(
  (
    ("depressed" "suicide" "exams" "university")
    (
      (when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)
      (you need to rest more often)
      (try to find a hobby)
    )
  )
  
  ( 
    ("mother" "father" "parents" "brother" "sister" "uncle" "ant" "grandma" "grandpa")
      (
        (tell me more about your * , i want to know all about your *)
        (why do you feel that way about your * ?)
        (how do you feel about your * ?)
        (i think it is misunderstanding between you and your *)
      )
  )
  (
    ("university" "scheme" "lections")
	(
	  (your education is important)
	  (how many time do you spend to learning ?)
          (had a difficult period with your education?)
          (try to change the way you study)
	)
  )
  (
    ("drugs" "alcohol" "smoke" "smoking")
    (
       (it is not worth your health)
       (you are trying to escape the problem not to solve it)
       (* is not a way out)
       (i understand that it is difficult for you, but you should not think about *)
    )
  )
  (
    ("work" "boss" "office" "profession")
    (
       (work has to inspire you)
       (do you feel that your work doesn`t bring you enjoy anymore ?)
       (maybe you should think about vacation ?)
       (what inspires you ?)
    )
  ) 
 )
)
			
; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (why do you suppose that)
                                       (why are you sure that)
                                       (in your opinion))
                )
                (change-person user-response)
        )
 )

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vctr)
  (vector-ref vctr (random (vector-length vctr)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace-v3 '(("am" "are")
                        ("are" "am")
                        ("i" "you")
                        ("me" "you")
                        ("mine" "yours")
                        ("my" "your")
						("myself" "yourself")
                        ("you" "i")
                        ("your" "my")
                        ("yours" "mine")
						("yourself" "myself")
						("we" "you")
						("us" "you")
						("our" "your")
						("ours" "yours")
						("ourselves" "yourselves")
						("yourselves" "ourselves")
						("shall" "will"))
                      phrase)
 )
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается прежнее начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
  )

; part 1
(define (many-replace-v2 replacement-pairs lst)
  (let loop ( (lst lst) (result '()) )
    (if (null? lst) (reverse result)
        (let ( (pat-rep (assoc (car lst) replacement-pairs)) )
          ( loop (cdr lst) (cons (if pat-rep (cadr pat-rep) (car lst)) result) )
          )
        )
    )   
  )

; part 1
(define (many-replace-v3 replacement-pairs lst)
   ( map ( lambda (x) (let ( (pat-rep (assoc x replacement-pairs)) ) (if pat-rep (cadr pat-rep) x)) ) lst )
  )

; 3й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (i understand your feelings)
                              (your feelings are very important to me)
                              (i see how difficult it is for you))
         )
)

; part 1
; 4й способ генерации овтетной реплики -- вернуться к сказанному пациентом ранее
(define (history-answer history)
   (append '(earlier you said that) (change-person (pick-random-vector history)) )
  )


; part 3

; структура стратегии
; на первом месте предикат, на втором вес, затем сама стратегия
(define (struct-strategy func-predicat weight strategy)
  (if (and (procedure? func-predicat) (integer? weight) (> weight 0) (procedure? strategy))
      (vector-immutable func-predicat weight strategy)
      (void)
      )
  )

(define (struct-strategy-predicate strategy)
  (vector-ref strategy 0)
  )

(define (struct-strategy-weight strategy)
  (vector-ref strategy 1)
  )

(define (struct-strategy-strategy strategy)
  (vector-ref strategy 2)
  )

(define (struct-strategy-null? strategy)
  (= (vector-length strategy) 0)
  )

(define (struct-strategy? strategy)
  (and (vector? strategy) (= (vector-length strategy) 3) (procedure? (vector-ref strategy 0))
       (integer? (vector-ref strategy 1)) (> (vector-ref strategy 1) 0) (procedure? (vector-ref strategy 2)))
  )


; структура для всех стратегий
; принимает любое число стратегий
(define (all-strategy-struct . tail)
  (if (andmap struct-strategy? tail)
      (list->vector tail)
      (void)
      )
  )


(define all-strategy-struct-length vector-length)

(define all-strategy-struct-ref vector-ref)

(define (all-strategy-struct-add all-strategy strategy)
  (if (struct-strategy?) (vector-append all-strategy (vector-immutable strategy)) all-strategy)
  )

(define (all-strategy-struct-delete all-strategy pos)
  (vector-append (vector-take all-strategy pos) (vector-drop all-strategy (add1 pos)))
  )



; tail: user-response, history, post-key-words, ... 
; функция, которая формирует список возможных стратегий
(define (available-strategy all-strategy args)
  ;(vector->list (vector-filter (lambda (x) ( (strategy-struct-pred x) args)) all-strategy))
  (let ( (length (all-strategy-struct-length all-strategy)) )
    (let loop ( (res '()) (pos 0) )
      (if (= pos length) res
          (let ( (strategy (all-strategy-struct-ref all-strategy pos)) )
            (if ((struct-strategy-predicate strategy) args) (loop (cons strategy res) (add1 pos)) (loop res (add1 pos)))
            )
          )
      )
    )
  )
            
    

; выбор случайного элемента с весом
; func - функция, достающая вес из элементов списка
(define (pick-random-with-weight list-of-elems-with-weight func)
  (let* ( (amount-weight (foldl (lambda (x y) (+ (func x) y)) 0 list-of-elems-with-weight))
          (result (random amount-weight)) )
    (let loop ( (result result) (lst list-of-elems-with-weight) )
      (let ( (new-res (- result (func (car lst)))) )
        (if (< new-res 0) (car lst) (loop new-res (cdr lst)))
        )
      )
    )
  )
  
; part3 обобщенный reply
; args: user-response, history, post-key-words, struct-graphs
(define (reply-v3 all-strategy . args)
  (let ((strategies (available-strategy all-strategy args)))
    ((struct-strategy-strategy (pick-random-with-weight strategies struct-strategy-weight)) args)
    )
  )


; структура из всех стратегий
(define (make-all-strategy-struct)
  
  ; оборачиваем стратегии в структуры
  (define qualifier-answer-struct
  (struct-strategy (lambda (args) #t) 2 (lambda (args) (qualifier-answer (pick-random-list (create-sentence-list (list-ref args 0)))))))

  (define hedge-struct
  (struct-strategy (lambda (args) #t) 1 (lambda (args) (hedge))))

  (define history-answer-struct
  (struct-strategy (lambda (args) (not (vector-empty? (list-ref args 1)))) 1 (lambda (args) (history-answer (list-ref args 1)))))

  (define key-word-answer-struct
  (struct-strategy (lambda (args) (find-key-words (list-ref args 0) (list-ref args 2))) 4
                      (lambda (args) (key-word-answer (list-ref args 0) (list-ref args 2))))
    )

  (define direct-gen-answer-struct
  (struct-strategy (lambda (args) #t) 3
                      (lambda (args) (direct-gen-answer (list-ref args 3))))
    )

  (define mixed-gen-answer-struct
  (struct-strategy (lambda (args) (not (null? (filter (lambda(x) (> (length x) N)) (create-sentence-list (list-ref args 0))))))
   4 (lambda (args) (mixed-gen-answer (list-ref args 3) (list-ref args 0))))
    )

  ; создаём структуру
  (all-strategy-struct qualifier-answer-struct hedge-struct history-answer-struct
                       key-word-answer-struct direct-gen-answer-struct mixed-gen-answer-struct)
  )

; part SPRING
; struct (grapg-start graph-direct graph-backward N)
(define (direct-gen-answer graphs)
  (direct-generation (car graphs) (cadr graphs))
  )

(define (mixed-gen-answer graphs-struct word-list)
  
  (define (choose-N-1-words sentences-list N)
    (let ( (possible-sentences (filter (lambda(x) (> (length x) N)) sentences-list)) )
      (let* ( (sentence (pick-random-list possible-sentences)) (pos (random (- (length sentence) N))) )
        (trunc-list (list-tail sentence pos) (sub1 N))
        )
      )
    )
   
    (mixed-generation (cadr graphs-struct) (caddr graphs-struct)
                       (change-person (choose-N-1-words (create-sentence-list word-list) (list-ref graphs-struct 3))))
  )

; структура графа
(define (make-graph-structure sentence-generator)
  (begin
    (fill-graph sentence-generator)
    (list graph-start graph-direct graph-backward N)
    )
  )

; сохранение графа в память
(define (save-graph-structure graph-struct filename)  
  (write-to-file graph-struct filename #:exists 'truncate)
  )

; загрузка из памяти
(define (load-graph-structure filename)
  (file->value filename)
  )
               



;END
(define (visit-doctor-v3 stop-word amount all-strategy graph-struct . tail)
  (let ( (post-key-words (if (null? tail) (prepare keywords_structure) (car tail) ) ) ) 
   (if (> amount 0)
      (let ( (name (ask-patient-name) ) )
        (if (not (equal? name stop-word))
            (begin                                   
             (printf "Hello, ~a!\n" name)
             (printf "what seems to be the trouble?")
             (doctor-driver-loop-v3 all-strategy name #() post-key-words graph-struct)
             (visit-doctor-v3 stop-word (sub1 amount) all-strategy post-key-words)
            )
            (void)
        )
      )
      (void)
     )
   )
)


;печать списка
(define (print-lst lst)
  (void (map (lambda (x) (begin (display x) (display " "))) lst))
  )

; part 3
(define (doctor-driver-loop-v3 all-strategy name history post-key-words graph-struct)
    (newline)
    (printf "- ") ; доктор ждёт ввода реплики пациента, приглашением к которому является -
    (let ( (user-response (split-string (read-line))) )
      (cond 
	    ((equal? (car user-response) "goodbye") ; реплика "goodbye" служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (printf "see you next week\n"))
            (else
                  (print-lst (cons "-" (reply-v3 all-strategy user-response history post-key-words graph-struct))) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop-v3 all-strategy name
                                (vector-append history (vector (pick-random-list (create-sentence-list user-response)))) post-key-words graph-struct)
             )
       )
      )
)


; part string
; список списков(где вложенный список соответствует предложению)
(define (create-sentence-list word-list (separators-list (list "!" "." "?")))
  (let loop ( (res '()) (buf '()) (word-list word-list))
    (if (null? word-list) (if (null? buf) res (cons (reverse buf) res))
        (if (ormap (lambda (x) (equal? x (car word-list))) separators-list)
            (loop (cons (reverse (cons (car word-list) buf)) res) '() (cdr word-list))
            (loop res (cons (car word-list) buf) (cdr word-list))
            )
        )
    )
  )     

; LAUNCH

;part 2 launch
;(visit-doctor-v2 'stop 2)

;SPRING launch
(random-seed (modulo (date->seconds (current-date)) 100))

(define sentence-generator (get-sentence "test.txt" (list "." "!" "?") ))
(define graph-struct (make-graph-structure sentence-generator))
(display "GRAPH IS FILLED\n")
;(save-graph-structure graph-struct "save_good_train.bin")
;(define graph-struct (load-graph-structure "save_bad_train.bin"))

(visit-doctor-v3 "stop" 2 (make-all-strategy-struct) graph-struct)






