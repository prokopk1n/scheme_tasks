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

(define (visit-doctor-2 name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name #())
)

; запускает доктора для работы с amount клиентов или до того, пока кто-либо
; не назовет в качестве имени стоп-слово
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
      (case (random 3) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((1) (qualifier-answer user-response)) ; 1й способ
          ((2) (hedge))  ; 2й способ
          ((3) (history-answer)); 3й способ
        
      )
)

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

; вспомогательная функция
; проверяет пересечение двух списков (есть ли одинаковые элементы)
(define (list-cross? lst1 lst2)
  (ormap (lambda (y) (member y lst2)) lst1)
  )

; вспомогательная функция
; выбор случайного элемента из списка
(define (pick-random-list lst)
  (list-ref lst (random (length lst)))
  )

; проверяет, есть ли ключевое слово в ответе
(define (find-key-words user-response post-key-words)
  (list-cross? user-response post-key-words)
  )



; 1й способ - генерация ответа по ключевым словам
(define (key-word-answer user-response post-key-words)
  (select-reply-expression user-response post-key-words)
  )

; выбор ответной реплики по ключевому слову
(define (select-reply-expression user-response post-key-words)
  (let* ( (key-word (select-key-word user-response post-key-words))
         (vector-expressions (vector-filter (lambda (x) (member key-word (car x))) keywords_structure) ) )
    (many-replace-v3 (list (list '* key-word)) (pick-random-list (pick-random-vector (vector-map (lambda (x) (cadr x) ) vector-expressions)))))
  )

; выбор ключевого слова из реплики
(define (select-key-word user-response post-key-words)
  (pick-random-list (make-key-words-list user-response post-key-words))
  )
    

;формирует список из ключевых слов в реплике
(define (make-key-words-list user-response post-key-words)
  (foldl (lambda (x y) (if (member x post-key-words)
                           (cons x y)
                           y)) '() user-response)
  )

                            
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


(define (many-replace-v2 replacement-pairs lst)
  (let loop ( (lst lst) (result '()) )
    (if (null? lst) (reverse result)
        (let ( (pat-rep (assoc (car lst) replacement-pairs)) )
          ( loop (cdr lst) (cons (if pat-rep (cadr pat-rep) (car lst)) result) )
          )
        )
    )   
  )


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


;4й способ генерации овтетной реплики -- вернуться к сказанному пациентом ранее
(define (history-answer history)
   (append '(earlier you said that) (change-person (pick-random-vector history)) )
  )


; ТЕСТЫ
(make-key-words-list '(scheme privet depressed kakaak scheme) '(depressed suicide exams university scheme))
(select-key-word '(scheme privet depressed kakaak scheme) '(depressed suicide exams university scheme))

(select-reply-expression'(scheme privet depressed kakaak scheme) '(depressed suicide exams university scheme))
