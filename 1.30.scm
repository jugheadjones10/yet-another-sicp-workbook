;Exercise 1.30

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum-iter cube inc b))
