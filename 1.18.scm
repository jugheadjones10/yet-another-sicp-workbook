;Exercise 1.18
;A solution more efficient than the one below, one that takes roughly half the number of steps, exists.

(define (iter-fast-mult a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))

  (define (iter-fast-mult-helper a b x)
    (cond ((= b 1) x)
          ((even? b) (iter-fast-mult-helper a (halve b) (+ (double a) x)))
          (else (iter-fast-mult-helper a (- b 1) (+ a x)))))
  (iter-fast-mult-helper a b 0))
