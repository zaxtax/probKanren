(load "../src/random-primitives.scm")
(load "../src/probKanren.scm")
(load "../src/prob-miniKanren-wrappers.scm")
(load "../src/postprocess.scm")


(define (appendo l s out)
  (conde
    ((== '() l) (== s out))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res)))))

(define samples (run 1024 (q)
		  (fresh (x y)
		    (== `(,x ,y) q)
		    (appendo x y '(1 2 3 4 5)))))

;(output-for-histogram (map (lambda (x) (cons x 1.0)) samples))

(define c (count samples))
