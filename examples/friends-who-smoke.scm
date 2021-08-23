(load "../src/random-primitives.scm")
(load "../src/probKanren.scm")
(load "../src/prob-miniKanren-wrappers.scm")
(load "../src/postprocess.scm")

(define (friends a b p)
  (conde
   ((== a 'ann) (== b 'bob) (== p #t))
   ((== a 'bob) (== b 'carl) (== p #t))
   ((== p #f))))
   
(define (stress person)
  (flip 0.3 person))

(define (influences person)
  (flip 0.2 person))

(define (smokes a a-smokes)
  (conde
   ((fresh (a-stressed)
      (stress a-stressed) (== a-stressed #t) (== a-smokes #t)))
   ((flip 0.5 a-smokes)
    (fresh (b b-smokes p)
      (smokes b b-smokes)
      (friends b a p)
      (constrain p)))))

(define (constrain p)
  (conde
   ((== p #t) (flip 0.75 #t))
   ((flip 0.25 #t))))

;(define samples (run 2 (q) (smokes 'carl q)))
