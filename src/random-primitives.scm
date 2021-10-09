;; Vendored from chez-stats

(define (check-p p who)
  (unless (and (and (>= p 0) (<= p 1)) (real? p))
    (assertion-violation who "p is not a real number in [0,1]")))

(define (check-real x x-name who)
  (unless (real? x)
    (assertion-violation who (string-append x-name " is not a real number"))))

(define (check-positive-real x x-name who)
  (unless (and (real? x) (> x 0))
    (assertion-violation who (string-append x-name " is not a positive real number"))))

(define (check-real-gte-zero x x-name who)
  (unless (and (real? x) (>= x 0))
    (assertion-violation who (string-append x-name " is not a real number >= 0"))))

(define (check-integer-gte-zero x x-name who)
  (unless (and (integer? x) (>= x 0))
    (assertion-violation who (string-append x-name " is not an integer >= 0"))))

(define (check-list lst lst-name who)
  (unless (list? lst)
    (assertion-violation who (string-append lst-name " is not a list")))
  (unless (for-all real? lst)
    (assertion-violation who (string-append "at least one element of " lst-name " is not a real number")))
  (when (null? lst)
    (assertion-violation who (string-append lst-name " is empty"))))

(define (bound logp . preds)
  (cond
   [(null? preds) logp]
   [(car preds) (apply bound (cons logp (cdr preds)))]
   [else -inf.0]))
  
(define pi (* (asin 1) 2))

(define (repeat n thunk)
  (let loop ([i 0]
             [result '()])
    (if (= i n)
        result
        (loop (add1 i) (cons (thunk) result)))))

;; equivalent result to (apply + (repeat...))
;; but presumably more efficient because not building up a list
;; and requires that the thunk returns a number
;; only used in random-binomial
(define (repeat-sum n thunk)
  (let loop ([i 0]
             [result 0])
    (if (= i n)
        result
        (loop (add1 i) (+ (thunk) result)))))

(define (cumulative-sum lst)
  (check-list lst "lst" "(cumulative-sum lst)")
  (let loop ([lst lst]
	     [result '()]
	     [total 0])
    (if (null? lst)
	(reverse result)
	(let ([new-total (+ (car lst) total)])
	  (loop (cdr lst) (cons new-total result) new-total)))))

(define (ess weights)
  (let ((n (fold-left (lambda (x y) (+ x (* y y))) 0 weights))
	(m (fold-left (lambda (x y) (+ x y)) 0 weights)))
    (/ (* m m) n)))

;; rescale ps (if necessary)
(define (scale-ps ps)
  (let ([p-sum (apply + ps)])
    (if (= p-sum 1)
	ps
	 (map (lambda (p) (/ p p-sum)) ps))))

;; If log-weights are all likely to be small, rescale them to prevent underflow
(define (rescaled-exp log-weights)
  (let ((t (max log-weights)))
    (map (lambda (w) (exp (- w t))) log-weights)))

(define (rescaled-exp-normed log-weights)
  (scale-ps (rescaled-exp log-weights)))

;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
(define (random-bernoulli p)
  (check-p p "(random-bernoulli p)")
  (if (<= (random 1.0) p) 1.0 0.0))

(define (logp-bernoulli p b)
  ;(check-p p "(logp-bernoulli p)")
  (if (= b 1) (log p) (log (- 1 p))))

(define random-normal
  (case-lambda
   [() (rnorm 0 1)]
   [(mu) (rnorm mu 1)]
   [(mu sd) (rnorm mu sd)]))

(define (logp-normal mu sd x)
  (check-real mu "mu" "(logp-normal mu sd x)")
  (check-real x "x" "(logp-normal mu sd x)")
  (check-positive-real sd "sd" "(logp-normal mu sd x)")
  (let [(tau (/ 1 (* sd sd)))]
    (/ (+ (* (- tau) (expt (- x mu) 2))
	  (log (/ tau pi 2.0)))
       2.0)))

;; rejection method from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
(define (rnorm mu sd)
  (let ([proc-string "(random-normal mu sd)"])
    (check-real mu "mu" proc-string)
    (check-real-gte-zero sd "sd" proc-string))
  (let loop ()
    (let* ([u1 (random 1.0)]
           [u2 (random 1.0)]
           [x (* -1 (log u1))])
      (if (> u2 (exp (/ (* -1 (expt (sub1 x) 2)) 2)))
          (loop)
          (let ([u3 (random 1.0)])
            (if (> u3 0.5)
                (+ mu (* sd x))
                (- mu (* sd x))))))))

(define (random-uniform mn mx)
  (let ([proc-string "(random-uniform mn mx)"])
    (check-real mn "mn" proc-string)
    (check-real mx "mx" proc-string)
    (unless (> mx mn)
      (assertion-violation proc-string "mx is not greater than mn")))
  (+ mn (* (- mx mn) (random 1.0))))

(define (logp-uniform lo hi x)
  (check-real lo "lo" "(logp-uniform lo hi x)")
  (check-real hi "hi" "(logp-uniform lo hi x)")
  (check-real x "x" "(logp-normal mu sd x)")
  (bound (- (log (- hi lo)))
	 (>= x lo)
	 (<= x hi)))
  
;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pd
(define (random-binomial trials p)
  (let ([proc-string "(random-binomial trials p)"])
    (check-integer-gte-zero trials "trials" proc-string)
    (check-p p proc-string))
  (repeat-sum trials (lambda () (random-bernoulli p))))

;; same algorithm as rmultinom in R
;; https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Multinom.html
(define (random-multinomial trials ps)
  (let ([proc-string "(random-multinomial trials ps)"])
    (check-integer-gte-zero trials "trials" proc-string)
    (check-list ps "ps" proc-string))
  (let loop ([p-scaled (scale-ps ps)]
             [p-used '()]
             [results '()])
    (if (null? p-scaled)
        (reverse results)
        (let* ([p-now (car p-scaled)]
               [p-now-adj-temp (/ p-now (- 1 (apply + p-used)))]
               [p-now-adj (if (> p-now-adj-temp 1) 1 p-now-adj-temp)]
               [result-next (random-binomial (- trials (apply + results)) p-now-adj)])
          (loop (cdr p-scaled) (cons p-now p-used) (cons result-next results))))))

;; TODO: Replace weights on particles with W_n/N
(define (resampling-draws counts particles)
  (cond
   [(null? counts) '()]
   [(zero? (car counts)) (resampling-draws (cdr counts) (cdr particles))]
   [else
    (cons (car particles) (resampling-draws
			   (cons (sub1 (car counts)) (cdr counts))
			   particles))]))

(define resample-multinomial
  (case-lambda
   [(particles) (resample-multinomial particles (length particles))]
   [(particles n)
    (let ((weights (rescaled-exp (map (lambda (x) (cdr x)) particles))))
      (let ((counts (random-multinomial n weights)))
	(resampling-draws counts particles)))]))

(define resample-residual
  (case-lambda
   [(particles) (resample-residual particles (length particles))]
   [(particles n)
    (let* ((weights
	    (rescaled-exp-normed (map (lambda (x) (cdr x)) particles)))
	   (first-counts
	     (map (lambda (w) (exact (floor (* w n)))) weights))
	   (R (fold-left + 0 first-counts)))
      (if (> (- n R) 0)
	  (resampling-draws first-counts particles)
	  (let* ((resid-weights
		  (map (lambda (w c) (/ (- (* n w) c) (- n R))) weights first-counts))
		 (resid-counts (random-multinomial (- n R) resid-weights)))
	    (resampling-draws
	     (map + first-counts resid-counts)
	     particles))))]))

(define (find-stratified-indices cweights l n i j acc)
  (cond
   [(= n i) (reverse (cons (length j) acc))]
   [else
    (let ((p (* l (/ (+ (random 1.0) i) n))))
      (let loop ([j-new j]
		 [cweights-new cweights]
		 [acc-new acc])
	(if (< p (car cweights-new))
	    (find-stratified-indices
	     cweights-new l n (add1 i) (cons '() j-new) acc-new)
	    (loop '() (cdr cweights-new) (cons (length j-new) acc-new)))))]))

(define resample-stratified
  (case-lambda
   [(particles) (resample-stratified particles (length particles))]
   [(particles n)
    (let ((weights (rescaled-exp (map (lambda (x) (cdr x)) particles))))
      (let ((cweights (cumulative-sum weights))
	    (l (apply + weights)))
	(let ((counts (find-stratified-indices cweights l n 0 '() '())))
	  (resampling-draws counts particles))))]))

(define (find-systematic-indices cweights l u n i j acc)
  (cond
   [(= n i) (reverse (cons (length j) acc))]
   [else
    (let ((p (* l (/ (+ u i) n))))
      (let loop ([j-new j]
		 [cweights-new cweights]
		 [acc-new acc])
	(if (< p (car cweights-new))
	    (find-systematic-indices
	     cweights-new l u n (add1 i) (cons '() j-new) acc-new)
	    (loop '() (cdr cweights-new) (cons (length j-new) acc-new)))))]))

(define resample-systematic
  (case-lambda
   [(particles) (resample-systematic particles (length particles))]
   [(particles n)
    (let ((weights (rescaled-exp (map (lambda (x) (cdr x)) particles))))
      (let ((cweights (cumulative-sum weights))
	    (l (apply + weights)))
	(let ((counts (find-systematic-indices cweights l (random 1.0) n 0 '() '())))
	  (resampling-draws counts particles))))]))
