;Exercise 1.31

;Part a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (identity x) x)
(define (factorial n)
  (product identity 1 inc n))

;The below is quite non-optimal. There are ways to include the
;"stray" 2 at the beginning into the pi-term procedure.
(define (approx-pi accuracy)
  (define (pi-term a)
    (/ (square (+ a 2)) (square (+ a 1))))
  (define (pi-next a)
    (+ a 2))
  (* (product pi-term 2 pi-next (* accuracy 2))
     (/ 8 (+ 3 (* accuracy 2)))))

;Part b
(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (iter-factorial n)
  (iter-product identity 1 inc n))
