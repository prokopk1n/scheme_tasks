#lang scheme
(define (colinear? x1 y1 z1 x2 y2 z2)
  (and (=(-(* y1 z2)(* z1 y2)) 0)
       (=(-(* x1 z2)(* z1 x2)) 0)
       (=(-(* x1 y2)(* y1 x2)) 0)))

(colinear? 1 1 1 2 2 2)
(colinear? 2 3 4 1 2 3)
(colinear? 1 0 1 -2 0 -2)
(colinear? 0 0 0 1 2 3)
