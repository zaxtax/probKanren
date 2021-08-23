(load "../src/random-primitives.scm")
(load "../src/probKanren.scm")
(load "../src/prob-miniKanren-wrappers.scm")
(load "../src/postprocess.scm")

;; Adapted from https://dtai.cs.kuleuven.be/problog/tutorial/tutslides/01_basic.html
;; Adapted from https://dtai.cs.kuleuven.be/problog/tutorial/basic/05_smokers.html

(define (friends a b)
  (conde
   ((== a 'ann) (== b 'bob))
   ((== a 'bob) (== b 'carl))))
   
(define (stress person)
  (bern 0.3 person))

(define (influences person)
  (bern 0.2 person))

(define (smokes a a-smokes)
  (conde
   ((fresh (a-stressed)
      (stress a-stressed) (== a-stressed a-smokes)))
   ((bern 0.5 a-smokes)
    (fresh (b b-smokes)
      (friends b a)
      (smokes b b-smokes)
      (constrain a-smokes b-smokes)))))

;; (define (constrain a b)
;;   (conde
;;    ((bern 0.75 1) (== a b))
;;    ((bern 0.25 1)
;;     (conde
;;      ((== a 0) (== b 1))
;;      ((== a 1) (== b 0))))))

;; (define (constrain a b)
;;   (conde
;;    ((bern 0.75 1) (== a b))
;;    ((bern 0.25 1) (== a 0) (== b 1))
;;    ((bern 0.25 1) (== a 1) (== b 0))))

;; (define (constrain a b)
;;   (conde
;;    ((bern 0.75 1) (== a b))
;;    ((bern 0.25 1) (=/= a b))))

(define (constrain a b)
  (conde
   ((bern 0.75 1) (== a b))
   ((bern 0.25 1))))

(define samples (run-with-p 300 (q) (smokes 'carl q)))
(empirical samples)
