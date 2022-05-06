(load "../src/random-primitives.scm")
(load "../src/probKanren.scm")
(load "../src/prob-miniKanren-wrappers.scm")
(load "../src/postprocess.scm")

(define (datapoint-likelihood c u1 u2 obs)
  (fresh (b)
    (bern c b)
    (conde
      ((== b 0.0) (normal u1 1 obs))
      ((== b 1.0) (normal u2 1 obs)))))

(define (dataset-likelihood c u1 u2 data)
  (fresh (a d)
    (datapoint-likelihood c u1 u2 1)
    (datapoint-likelihood c u1 u2 0)
    (datapoint-likelihood c u1 u2 1)
    (datapoint-likelihood c u1 u2 0)
    (datapoint-likelihood c u1 u2 1)
    (datapoint-likelihood c u1 u2 0)
    (datapoint-likelihood c u1 u2 1)
    (datapoint-likelihood c u1 u2 0)
    (datapoint-likelihood c u1 u2 1)
    (datapoint-likelihood c u1 u2 0)
    (datapoint-likelihood c u1 u2 1)
    (datapoint-likelihood c u1 u2 0)
    (datapoint-likelihood c u1 u2 1)
    (datapoint-likelihood c u1 u2 0)
    (datapoint-likelihood c u1 u2 1)
    (datapoint-likelihood c u1 u2 0)
    (datapoint-likelihood c u1 u2 1)
    (datapoint-likelihood c u1 u2 0)
    (datapoint-likelihood c u1 u2 1)
    (datapoint-likelihood c u1 u2 0)

    #;(conde
      ((== data '()))
      (
       (== data (cons a d)) ;For each datapoint,       
       (datapoint-likelihood c u1 u2 a) ; compute its likelihood
       (dataset-likelihood c u1 u2 d)
       ))))

(define (gaussian-mixture q data)
  (fresh (c u1 u2) ; class-prob, means 1 & 2
    (== q (list c u1 u2)) ; return all random vars via the 1 query parameter
    (uniform 0 1 c) ; Sample prior class probability from uniform
    (normal 0 1 u1) ; Sample prior means 
    (normal 1 1 u2)
    (dataset-likelihood c u1 u2 data)
    ))

; 2 Gaussians with means 0 and 1
(define data '(1 0 1 0 1 0))			
(define samples (run-with-p 1000000 (q) (gaussian-mixture q data)))

; extract (class-prob . weight) pairs
(define class-prob (map (lambda (s) (cons (caar s) (cdr s))) samples))

;(display samples)
;(display class-prob)
(output-for-histogram class-prob)
