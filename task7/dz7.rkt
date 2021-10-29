#lang scheme
(require racket/mpair)

#|(define (rot-left-func! mlst)
  (let loop ( (i (sub1 (mlength mlst))) (tail mlst) )
    (if (> i 0)
        (let ( (buf (mcar tail)) )
          (begin
            (set-mcar! tail (mcar (mcdr tail)))
            (set-mcdr! tail (let ((tail_of_tail (mcdr tail))) (begin (set-mcar! tail_of_tail buf) tail_of_tail)) )
            (loop (sub1 i) (mcdr tail))
            )
          )
        mlst
        )
    )
  )|#


(define (rot-left-func! mlst)
  (if (or (null? mlst) (not (mpair? mlst)) (null? (mcdr mlst))) mlst
      (let ( (first (mcar mlst)) )
        (let loop ( (mlst-tail mlst) )
          (if (null? (mcdr mlst-tail)) (set-mcar! mlst-tail first)
              (begin
                (set-mcar! mlst-tail (mcar (mcdr mlst-tail)))
                (loop (mcdr mlst-tail))
                )
              )
          )
        )
      )
  )


(define l (mlist 1 2 3 4 5))
(rot-left-func! l)
l



(define-syntax rot-left!
  (syntax-rules ()
    ((rot-left! mlst)
     (if (or (null? mlst) (not (mpair? mlst)) (null? (mcdr mlst))) mlst
      (let ( (first (mcar mlst)) )
        (let loop ( (mlst-tail mlst) )
          (if (null? (mcdr mlst-tail)) (set-mcar! mlst-tail first)
              (begin
                (set-mcar! mlst-tail (mcar (mcdr mlst-tail)))
                (loop (mcdr mlst-tail))
                )
              )
          )
        )
      ) )
    )
  )

(rot-left! l)
l
         
         