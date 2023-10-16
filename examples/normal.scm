(load "../src/random-primitives.scm")
(load "../src/probKanren.scm")
(load "../src/prob-miniKanren-wrappers.scm")
(load "../src/postprocess.scm")

(define (normal-normal q)
  (fresh (x)
    (normal 0 1 x)
    (normal x 1 q)))

(define (normal-mixture q)
  (conde
   ((normal -7 1 q))
   ((normal 0 1 q))
   ((normal 7 1 q))))

(define (normal-mixture2 q)
  (fresh (x)
   (conde
    ((== x 0))
    ((== x 7)))
   (normal x 1 q)))

(define (normal-cond theta)
  (fresh ()
    (normal 0 1 theta)
    (normal theta 1 4)))

(define (normal-cond2 theta)
  (fresh ()
    (== theta 0.5)
    (normal theta 1 4)
    (normal 0 1 theta)))

(define samples
  (run-with-p 100 (q)
     (normal-mixture2 q)))

;(define samples
;  (run-with-p 10 (q) (fresh (p) (conj (uniform 0.8 1 p) (bern p q)))))

(output-for-histogram samples)
