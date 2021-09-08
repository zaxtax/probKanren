(define (empirical-mean samples)
  (/ (fold-left (lambda (x y) (+ x (* (car y) (cdr y)))) 0 samples)
     (fold-left (lambda (x y) (+ x (cdr y))) 0 samples)))

(define (empirical-variance samples mean)
  (let ((n (length samples)))
    (if (< n 2)
	(error "empirical-variance" "must have at least two samples")
	(/ (fold-left (lambda (x y) (+ x (* (cdr y)
					    (expt (- (car y) mean) 2))))
		      0 samples)
	   (sub1 n)))))

(define (output-for-histogram samples)
  (map (lambda (s) (printf "~f\t~s\n" (cdr s) (car s)))
       samples))

(define (count samples)
  (count-with-tbl samples '()))

(define (count-with-tbl samples tbl)
  (cond
   [(null? samples) tbl]
   [else (let ((tbl (update-tbl (car samples) tbl)))
	   (count-with-tbl (cdr samples) tbl))]))

(define (update-tbl x tbl)
  (cond
   [(null? tbl) (cons (cons x 1) tbl)]
   [(equal? (caar tbl) x) (cons (cons x (add1 (cdar tbl))) (cdr tbl))]
   [else (cons (car tbl) (update-tbl x (cdr tbl)))]))
