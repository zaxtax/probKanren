(load "../src/random-primitives.scm")
(load "../src/probKanren.scm")
(load "../src/prob-miniKanren-wrappers.scm")
(load "../src/postprocess.scm")

(define (beta-bernoulli p) ;; p(1 - p)
  (fresh ()
    (bern p 0.0)    ;; p2(x1 = 0 | p) = p
    (bern p 1.0)
    (uniform 0 1 p) ;; p1(p) = 1 if 0 < p < 1 else 0
    ))  ;; p3(x2 = 1 | p) = 1 - p

(define (beta-bernoulli2 p) ;; p(1 - p)
  (fresh ()
    (bern p 0.0)    ;; p2(x1 = 0 | p) = p
    (bern p 1.0)
    ))  ;; p3(x2 = 1 | p) = 1 - p

(define (beta-bernoulli3 p) ;; p(1 - p)
  (fresh ()
    (bern p p)    ;; p2(x1 = x | y) = if p == 0 then 0 else 1 - p
    ))  ;; p3(x2 = 1 | p) = 1 - p

(define (beta-bernoulli4 p q) ;; p(1 - p)
  (fresh ()
    (bern p q)    ;; p2(x1 = x | y) = if p == 0 then 0 else 1 - p
    ))  ;; p3(x2 = 1 | p) = 1 - p

(define (beta-0 p q1 q2)
  (fresh ()
   (uniform 0 1 p)
   (bern p q1)
   (bern p q2)))

(define samples-0
  (run-with-p 20 (p)
   (fresh () (beta-0 p 0.0 1.0))))

(define samples
  (run-with-p 20 (q)
	      (fresh (p)
		     (== p q)
		     (beta-bernoulli4 p q)
		     )))

(output-for-histogram samples-0)
