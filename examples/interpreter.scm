(load "../probmk/mk-vicare.scm")
(load "../probmk/random-primitives.scm")
(load "../probmk/mk.scm")
(load "../probmk/postprocess.scm")

(define (delete-duplicates xs)
  (let ((seen (make-hash-table equal?)))
    (let loop ((xs xs) (new-list '()))
      (if (null? xs)
          (reverse new-list)
          (loop (cdr xs)
                (let ((x (car xs)))
                  (if (hash-table-contains? seen x)
                      new-list
                      (begin (hash-table-set! seen x #t)
                             (cons x new-list)))))))))

(define evalo
  (lambda (expr val)
    (eval-expro expr '() val)))

(define eval-expro
  (lambda (expr env val)
    (_conde-p
      ((bern 0.15 1.0)
       (symbolo expr) (lookupo expr env val))
      ((bern 0.5 1.0)
       (_conde-p
         ((bern 0.00000001 1.0)
          (fresh (a d)
            (== val (cons a d))))
         ((bern 1.0 1.0)
          (symbolo val))
         ((bern 1.0 1.0)
          (numbero val))
         ((bern 1.0 1.0)
          (== val '())))
       (== `(quote ,val) expr)
       (not-in-envo 'quote env)
       (absento 'closure val))
      ((bern 1.0 1.0)
       (fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== (cons v1 v2) val)
         (not-in-envo 'cons env)
         (eval-expro e1 env v1)
         (eval-expro e2 env v2)))
      ((bern 0.20 1.0)
       (fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (== `(closure ,x ,body ,env) val)
         (not-in-envo 'lambda env)))
      ((bern 0.20 1.0)
       (fresh (rator rand x body env^ a)
         (== `(,rator ,rand) expr)
         (eval-expro rator env `(closure ,x ,body ,env^))
         (eval-expro rand env a)
         (eval-expro body `((,x . ,a) . ,env^) val)))
)))

(define not-in-envo
  (lambda (x env)
    (_conde
     ((bern 0.5 1.0)
      (== '() env))
     ((bern 0.5 1.0)
      (fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest))))))

(define lookupo
  (lambda (x env t)
    (_conde
     ((bern 0.5 1.0)
      (fresh (y v rest)
         (== `((,y . ,v) . ,rest) env) (== y x)
         (== v t)))
     ((bern 0.5 1.0)
      (fresh (y v rest)
         (== `((,y . ,v) . ,rest) env) (=/= y x)
         (lookupo x rest t))))))

(define (tests)
  (pretty-print (_run 5 (v) (evalo '(lambda (x) x) v)))
  ;; '((closure x x ()))

  (pretty-print (_run-with-p 5 (v) (evalo '(lambda (x) x) v)))
  ;; (((closure x x ()) . 0.10000000000000002))

  (pretty-print (_run 5 (v) (evalo ''1 v)))
  ;; (1)

  (pretty-print (_run 100 (v) (evalo '(cons '1 '()) v)))
  ;; ((1))

  (pretty-print
   (remove-duplicates
    (_run 50 (e)
      (evalo e '(I love)))))
  ;; nice

  (pretty-print
   (sort (lambda (x y) (> (cdr x) (cdr y)))
         (count-with-p
          (_run-with-p 50 (e)
                       (evalo e '(I love))))))
  ;; nice

  ;; (pretty-print
  ;;  (_run-with-p 2 (e)
  ;;    (evalo e e)))
  )

(tests)

