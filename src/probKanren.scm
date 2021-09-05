;; probKanren by Rob Zinkov and William Byrd
;; Jason Hemann and Dan Friedman
;; microKanren, final implementation from paper

(define (concat l) (apply append l))

(define (split-at l k)
  (define (split-at-aux l k acc)
    (cond
     [(null? l) (cons acc l)]
     [(zero? k) (cons acc l)]
     [else
      (split-at-aux (cdr l) (sub1 k) (cons (car l) acc))]))
  (split-at-aux l k '()))

(define (split-into l k)
  (define (split-into-aux l k acc)
    (let ((c/r (split-at l k)))
      (if (null? (cdr c/r))
          (cons (car c/r) acc)
          (split-into-aux (cdr c/r) k (cons (car c/r) acc)))))
  (reverse (split-into-aux l k '())))

(define *total-active-particles* 0)
(define *max-particles* 10000)

(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define get-s car)
(define get-l cdr)
(define get-c cdr)

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (per-particle particles f)
  (let ((s (remq 'fail (map f (car particles)))))
    (if (null? s)
	mzero
	(unit (cons s (cdr particles))))))

;; TODO: Resampling should go here
(define (== u v)
  (lambda (s/c)
    (per-particle s/c
      (lambda (s/l)
       (let ((s (unify u v (get-s s/l))))
         (if s `(,s . ,(get-l s/l)) 'fail))))))

(define (unit s/c) (cons s/c mzero))

(define mzero '())

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (equal? u v) s)))))

(define (ground? u s)
  (let ((t (walk u s)))
    (cond
      ((var? t) #f)
      ((pair? t)
        (and (ground? (walk (car t) s))
             (ground? (walk (cdr t) s))))
      (else #t))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (get-c s/c)))    
      ((f (var c)) `(,(get-s s/c) . ,(+ c 1))))))

(define (msum gs ss)
  (cond
   [(null? gs) mzero]
   [(null? ss) mzero]
   [else
    (mplus ((car gs) (car ss))
	   (msum (cdr gs) (cdr ss)))]))

(define (disj . gs)
  (lambda (s/c)
    (let* ((s (get-s s/c))
	   (c (get-c s/c))
	   (n (ceiling (/ (length s) (length gs))))
	   (chunks (split-into s n)))
      (msum gs (map (lambda (x) (cons x c)) chunks)))))

(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

(define (normal mu sd x)
  (lambda (s/c)
    (per-particle s/c
      (lambda (s/l)
        (let ((s (get-s s/l)))		    
	  (if (and (ground? mu s) (ground? sd s))
	      (let ((mu-g (walk mu s))
		    (sd-g (walk sd s)))
		(if (ground? x s)
		    `(,s .
		      ,(+ (logp-normal mu-g sd-g (walk x s))
			  (get-l s/l)))
		    (let ((xs (random-normal mu-g sd-g)))
		      `(,(ext-s x xs s) .
			,(get-l s/l)))))
	      (error "normal" "parameters are not ground")))))))

(define (bern p b)
  (lambda (s/c)
    (per-particle s/c
      (lambda (s/l)
        (let ((s (get-s s/l)))		    
	  (if (ground? p s)
	      (let ((p-g (walk p s)))
		(if (ground? b s)
		    `(,s .
		      ,(+ (logp-bernoulli p-g (walk b s))
			  (get-l s/l)))
		    (let ((bs (random-bernoulli p-g)))
		      `(,(ext-s b bs s) .
			,(get-l s/l)))))
	      (error "bern" "parameter is not ground")))))))
