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

(define (bound logp . preds)
  (cond
   [(null? preds) logp]
   [(car preds) (apply bound (cons logp (cdr preds)))]
   [else -inf.0]))
  
(define pi (* (asin 1) 2))

;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
(define (random-bernoulli p)
  (check-p p "(random-bernoulli p)")
  (if (<= (random 1.0) p) #t #f))

(define (logp-bernoulli p b)
  (check-p p "(logp-bernoulli p)")
  (if b (log p) (log (- 1 p))))

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

;; Marsaglia and Tsang’s Method
;; from https://www.hongliangjie.com/2012/12/19/how-to-generate-gamma-random-variables/
;; material at url refers to rate parameter as scale
(define (random-gamma shape rate)
  (let ([proc-string "(random-gamma shape rate)"])
    (check-positive-real shape "shape" proc-string)
    (check-positive-real rate "rate" proc-string))
  (let loop ([shape shape]
             [rate rate])
    (if (not (= rate 1))
        (/ (loop shape 1) rate)
        (if (< shape 1)
            (* (loop (add1 shape) rate)
               (expt (random 1.0) (/ 1 shape)))
            (let* ([d (- shape 1/3)]
                   [c (/ 1 (sqrt (* 9 d)))]
                   [Z (random-normal)]
                   [U (random 1.0)]
                   [V (expt (add1 (* c Z)) 3)]
                   [z-comp (/ -1 c)]
                   [log-U-comp (+ (* 1/2 (expt Z 2))
                                  (- d (* d V))
                                  (* d (log V)))])
              (if (and (> Z z-comp) (< (log U) log-U-comp))
                  (* d V)
                  (loop shape rate)))))))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
(define (random-beta a b)
  (let ([proc-string "(random-beta a b)"])
    (check-positive-real a "a" proc-string)
    (check-positive-real b "b" proc-string))
  (let ([A (random-gamma a 1)]
	[B (random-gamma b 1)])
    (/ A (+ A B))))

