;; chapter 2
(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

;; 2.1.1
;; make rational number
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (* n -1) g) (/ (* d -1) g))
        (cons (/ n g) (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (numer y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;; exercise 2.2
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment line-segment)
  (car line-segment))
(define (end-segment line-segment)
  (cdr line-segment))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (point-math point) (/ (+ (x-point point) (y-point point)) 2))
(define (midpoint-segment line)
  (cons (point-math (start-segment line))
        (point-math (end-segment line))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;; exercise 2.3
(define (make-rectange))
(define (rectangle-perimeter))
(define (rectangle-area))
