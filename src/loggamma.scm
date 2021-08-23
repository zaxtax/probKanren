;; Adapted from Racket's math-lib library

(define euler.0 2.718281828459045235360287471352662497759)

(define (log-gamma x)
  (cond [(flonum? x)  (fllog-gamma x)]
        [(single-flonum? x)  (fllog-gamma (fl x))]
        [(integer? x)
         (cond [(x . <= . 0)
                (raise-argument-error 'log-gamma "Real, not Zero or Negative-Integer" x)]
               [(eqv? x 1)  0]
               [else  (fllog-factorial (fl (- x 1)))])]
        [else  (fllog-gamma (fl x))]))
