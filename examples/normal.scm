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

(define (normal-cond theta)
  (fresh ()
    (normal 0 1 theta)
    (normal theta 1 4)))

(define samples (run-with-p 10000 (q) (normal-mixture q)))

(output-for-histogram samples)
