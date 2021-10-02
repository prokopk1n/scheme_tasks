; "Доктор". Осень 2021
#lang scheme/base
; В учебных целях используется базовая версия Scheme

(require racket/vector)
; подключаем функции для работы с векторами
 
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
  (doctor-driver-loop-v2 name #())
)

; part2
; запускает доктора для работы с amount клиентов или до того, пока кто-либо
; не назовет в качестве имени стоп-слов
; tail нужен для того, чтобы передавать предобработанную структуру
(define (visit-doctor-v2 stop-word amount . tail)
  (let ( (post-key-words (if (= (length tail) 0) (prepare keywords_structure) (car tail) ) ) ) 
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
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
)

; part 2
; функция, которая обрабатывает структуру, которая позволяет
; строить овтетные реплики по ключевым словам пациента
; данная функция строит список ключевых слов
(define (prepare structure)
  (let loop ( (n (vector-length structure)) (res '()) )
    ( if (= n 0) res
         (loop (sub1 n) (foldl (lambda (x y) (cons x y)) res (car (vector-ref structure (sub1 n))))) )
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
    (many-replace-v3 (list (list '* key-word)) (pick-random-list (pick-random-vector (vector-map (lambda (x) (cadr x) ) vector-expressions)))))
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
    (depressed suicide exams university)
    (
      (when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)
    )
  )
  
  ( 
    (mother father parents brother sister uncle ant grandma grandpa)
      (
        (tell me more about your * , i want to know all about your *)
        (why do you feel that way about your * ?)
      )
  )
  (
    (university scheme lections)
	(
	  (your education is important)
	  (how many time do you spend to learning ?)
	)
  )
  (
    (drugs alcohol smoke smoking)
    (
       (it is not worth your health)
       (you are trying to escape the problem not to solve it)
    )
  )
  (
    (work boss office profession)
    (
       (work has to inspire you)
       (do you feel that your work doesn`t bring you enjoy anymore)
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
        (many-replace-v3 '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
						(myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
						(yourself myself)
						(we you)
						(us you)
						(our your)
						(ours yours)
						(ourselves yourselves)
						(yourselves ourselves)
						(shall will))
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
; args: user-response, history, post-key-words, ...
(define (reply-v3 all-strategy . args)
  (let ((strategies (available-strategy all-strategy args)))
    ((struct-strategy-strategy (pick-random-with-weight strategies struct-strategy-weight)) args)
    )
  )


; структура из всех стратегий
(define (make-all-strategy-struct)
  
  ; оборачиваем стратегии в структуры
  (define qualifier-answer-struct
  (struct-strategy (lambda (args) #t) 2 (lambda (args) (qualifier-answer (list-ref args 0)))))

  (define hedge-struct
  (struct-strategy (lambda (args) #t) 1 (lambda (args) (hedge))))

  (define history-answer-struct
  (struct-strategy (lambda (args) (not (vector-empty? (list-ref args 1)))) 1 (lambda (args) (history-answer (list-ref args 1)))))

  (define key-word-answer-struct
  (struct-strategy (lambda (args) (find-key-words (list-ref args 0) (list-ref args 2))) 2
                      (lambda (args) (key-word-answer (list-ref args 0) (list-ref args 2))))
    )

  ; создаём структуру
  (all-strategy-struct qualifier-answer-struct hedge-struct history-answer-struct key-word-answer-struct)
  )

(define (visit-doctor-v3 stop-word amount all-strategy . tail)
  (let ( (post-key-words (if (= (length tail) 0) (prepare keywords_structure) (car tail) ) ) ) 
   (if (> amount 0)
      (let ( (name (ask-patient-name) ) )
        (if (not (equal? name stop-word))
            (begin                                   
             (printf "Hello, ~a!\n" name)
             (print '(what seems to be the trouble?))
             (doctor-driver-loop-v3 all-strategy name #() post-key-words)
             (visit-doctor-v3 stop-word (sub1 amount) all-strategy post-key-words)
            )
            (void)
        )
      )
      (void)
     )
   )
)

; part 3
(define (doctor-driver-loop-v3 all-strategy name history post-key-words)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else
                  (print (reply-v3 all-strategy user-response history post-key-words)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop-v3 all-strategy name (vector-append history (vector user-response)) post-key-words)
             )
       )
      )
)


; LAUNCH

;part 2 launch
;(visit-doctor-v2 'stop 2)

;part 3 launch
;(visit-doctor-v3 'stop 2 (make-all-strategy-struct))






