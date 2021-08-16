;Exercise 1.16

(define (fast-expt b n)
  (define (fast-expt-helper b n a)
    (cond ((= n 0) (* a))
          ((even? n) (fast-expt-helper (square b) (/ n 2) a))
          (else (fast-expt-helper b (- n 1) (* b a)))))
  (define (even? n)
    (= (remainder n 2) 0))
  (fast-expt-helper b n 1))
