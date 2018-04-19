;; factorial function
;; n! = n * [(n - 1) * (n - 2) *** 3 * 2 * 1] = n * (n - 1)!
(define (factorial n) ; recusive factorial - no tail recursion
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (tail-factorial n) ; recursive factorial - tail recursion
  (fact-iter 1 1 n))

(define (fact-iter  product counter max-counter)
  (if (> counter max-counter)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-counter)))

;; Exercise 1.9 - are these procedures iterative or recusive?
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; Exercise 1.10 - Ackerman's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; 1.2.2 Tree Recursion
