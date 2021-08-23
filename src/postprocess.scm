(define (empirical samples)
  (/ (fold-left (lambda (x y) (+ x (* (car y) (cdr y)))) 0 samples)
     (fold-left (lambda (x y) (+ x (cdr y))) 0 samples)))
     
(define (output-for-histogram samples)
  (map (lambda (s) (printf "~f\t~f\n" (car s) (cdr s)))
       samples))
