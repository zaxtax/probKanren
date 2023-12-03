(load "../probmk/mk-vicare.scm")
(load "../probmk/random-primitives.scm")
(load "../probmk/mk.scm")

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
    (conde
      ((bern 0.25 1.0)
       (symbolo expr) (lookupo expr env val))
      ((bern 0.25 1.0)
       (== `(quote ,val) expr)
       (not-in-envo 'quote env)
       (absento 'closure val))
      ((bern 0.15 1.0)
       (fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== (cons v1 v2) val)
         (not-in-envo 'cons env)
         (eval-expro e1 env v1)
         (eval-expro e2 env v2)))
      ((bern 0.10 1.0)
       (fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (== `(closure ,x ,body ,env) val)
         (not-in-envo 'lambda env)))
      ((bern 0.25 1.0)
       (fresh (rator rand x body env^ a)
         (== `(,rator ,rand) expr)
         (eval-expro rator env `(closure ,x ,body ,env^))
         (eval-expro rand env a)
         (eval-expro body `((,x . ,a) . ,env^) val)))
)))

(define not-in-envo
  (lambda (x env)
    (conde
      ((== '() env))
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest))))))

(define lookupo
  (lambda (x env t)
    (conde
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env) (== y x)
         (== v t)))
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env) (=/= y x)
         (lookupo x rest t))))))

(define (tests)
  (pretty-print (run 5 (v) (evalo '(lambda (x) x) v)))
  ;; '((closure x x ()))

  (pretty-print (run-with-p 5 (v) (evalo '(lambda (x) x) v)))
  ;; (((closure x x ()) . 0.10000000000000002))

  (pretty-print (run 5 (v) (evalo ''1 v)))
  ;; (1)

  (pretty-print (run 1000 (v) (evalo '(cons '1 '()) v)))
  ;; ((1))

  (pretty-print
   (remove-duplicates
    (run 10000 (e)
      (evalo e '(I love)))))
  ;; nice

  (pretty-print
   (remove-duplicates
    (run-with-p 10000 (e)
                (evalo e '(I love)))))
  ;; nice

  #;
(run 10000000 (e)
  (== e '((lambda (_.0)
            (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
          '(lambda (_.0)
             (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))))
  (evalo e e))
  )

(tests)

