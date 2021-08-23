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
   ((normal 0 1 q))
   ((normal 7 1 q))))

(define samples (run 10000 (q) (normal-normal q)))

(output-for-histogram
  (map (lambda (x) (cons 1.0 x)) samples))
