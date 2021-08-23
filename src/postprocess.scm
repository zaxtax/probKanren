(define (empirical samples)
  samples)

(define (output-for-histogram samples)
  (map (lambda (s) (printf "~f\t~f\n" (car s) (cdr s)))
       samples))
