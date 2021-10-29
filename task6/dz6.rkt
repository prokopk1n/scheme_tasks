#lang scheme
(require scheme/mpair)

(define (make-queue) (mlist '*queue*))

(define (queue? q)
  (and (mlist? q) (eq? (mcar q) '*queue*))
  )

(define (empty-queue? q)
  (and (queue? q) (null? (mcdr q)))
  )

(define (front-queue q)
  (if (and (queue? q) (not (empty-queue? q)))
      (mcar (mcar (mcdr q)))
      "Empty queue"
      )
  )


(define (insert-queue! q e)
  (let ( (newelem (mlist e)) )
    (if (empty-queue? q)
          (begin
            (set-mcdr! q (mlist newelem newelem))
            q)
          (let ((last (mlist-ref q 2)))
          (begin
            (set-mcdr! last (mcons e '()))
            (set-mcdr! q (mlist (mcar (mcdr q)) (mcdr last)))
            q)
          )  
        )
    )
  )

(define (delete-queue! q)
  (if (not (empty-queue? q))
      (if (eq? (mlist-ref q 1) (mlist-ref q 2))
          (set-mcdr! q '())
          (set-mcdr! q (mlist (mcdr (mlist-ref q 1)) (mlist-ref q 2)))
          )
      q
      )
  )



