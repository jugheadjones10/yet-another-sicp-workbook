;Exercise 2.7 - 2.16

(define (make-interval a b) (cons a b))
(define (upper-bound x) (car x))
(define (lower-bound x) (cdr x))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
       (make-interval (min p1 p2 p3 p4)
                      (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))
(define (div-interval-improved x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division error (y spans 0)")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))
(define (make-center-percent c p)
  (let ((width (* c (/ p 100.0))))
    (make-interval (- c width) (+ c width))))
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))
(define (center x)
  (/ (+ (upper-bound x) (lower-bound x)) 2.0))
(define (percent x)
  (* (/ (width x) (center x)) 100.0))
