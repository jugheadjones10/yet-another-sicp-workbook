;Exercise 1.43

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeat-recur n)
    (if (= n 1)
        f
        (compose f (repeat-recur (- n 1)))))
  (compose f (repeat-recur (- n 1))))
