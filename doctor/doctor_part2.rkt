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
  (let ( (post-key-words (if (= (length tail) 0) (prepare keywords_structure) tail)) ) 
   (if (> amount 0)
      (let ( (name (ask-patient-name) ) )
        (if (not (equal? name stop-word))
            (begin                                   
             (printf "Hello, ~a!\n" name)
             (print '(what seems to be the trouble?))
             (doctor-driver-loop-v2 name #())
             (visit-doctor-v2 stop-word (sub1 amount))
            )
            (void)
        )
      )
      (void)
     )
   )
)


(define (visit-doctor-v3 stop-word amount . tail)
  (let ( (post-key-words (if (= (length tail) 0) (prepare keywords_structure) tail)) ) 
   (if (> amount 0)
      (let ( (name (ask-patient-name) ) )
        (if (not (equal? name stop-word))
            (begin                                   
             (printf "Hello, ~a!\n" name)
             (print '(what seems to be the trouble?))
             (doctor-driver-loop-v2 name #())
             (visit-doctor-v2 stop-word (sub1 amount))
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

(define (doctor-driver-loop-v2 name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else
                  (print (reply-v2 user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop-v2 name (vector-append history (vector user-response)))
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
      (case (random 3) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
          ((2) (history-answer)); 3й способ
        
      )
)

; pre-struct предобработанная структура
; сама структура
(define (reply-v2 user-response history pre-struct struct)
      (case (if (vector-empty? history) (random 2) (random 3)) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
          ((2) (history-answer history)); 3й способ
        
      )
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
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

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
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


;3й способ генерации овтетной реплики -- вернуться к сказанному пациентом ранее
(define (history-answer history)
   (append '(earlier you said that) (change-person (pick-random-vector history)) )
  )



; 4й способ генерации на основе ключевых слов
; структура из ключевых слов и реплик-ответов
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




