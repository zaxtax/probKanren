(load "../src/random-primitives.scm")
(load "../src/probKanren.scm")
(load "../src/prob-miniKanren-wrappers.scm")
(load "../src/postprocess.scm")

(define (beta-bernoulli p)
  (fresh ()
    (uniform 0 1 p)
    (bern p 1.0)))

(define samples (run-with-p 10000 (q) (beta-bernoulli q)))

(output-for-histogram samples)
