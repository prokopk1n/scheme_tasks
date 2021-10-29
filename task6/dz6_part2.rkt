#lang scheme

(define (deficient? n)
  (let ( (buf (quotient n 2)) )
    (let loop ( (i 2) (sum 1) )
      (if (> i buf)
          (< sum n)
          (if (= (remainder n i) 0) (loop (+ i 1) (+ sum i)) (loop (+ i 1) sum))
          )
      )
    )
  )

(define (even-deficient n)
  (let loop ((i 1) (number 2))
    (if (deficient? number)
        (if (= i n) number (loop (add1 i) (+ number 2)))
        (loop i (+ number 2))
        )
    )
  )

(define deficient-table (make-hash '((1 . 2))))

(define (memo-even-deficient n)
  (let ( (res (hash-ref deficient-table n #f)) )
    (if (not res)
        (hash-ref (let loop ( (number (+ (memo-even-deficient (- n 1)) 2)) )
          (if (deficient? number) (begin (hash-set! deficient-table n number) deficient-table) (loop (+ number 2)))) n)
        res)
    )
  )

#|
Мемоизированная версия даёт выигрыш в тех ситуациях, когда мы вызываем её несколько раз и для номеров n,
разность между которыми невилика. Так, например, если мы вызовем функцию (memo-even-deficient n), а затем (memo-even-deficient (add1 n)),
то второй вызов отработает в лучшем случае за O(1), тогда как (even-deficient n) работает как минимум за O(n^2).
Однако если сравнивать (memo-even-deficient n) и (even-deficient n), то второй вызов работает быстрее, так как в even-deficient процесс вычисления
итеративный и не расходуется память на хэш-таблицу.
|#
