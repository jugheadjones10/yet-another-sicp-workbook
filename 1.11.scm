(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(define (f-iter n)
  (define (f-iter-helper three two one counter)
    (if (= counter 0)
        one
        (f-iter-helper two
                       one
                       (+ one
                          (* 2 two)
                          (* 3 three)
                          (- counter 1)))))
  (if (< n 3)
      n
      (f-iter-helper 0 1 2 (- n 2))))
