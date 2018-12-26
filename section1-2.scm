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

;; Exercise 1.10 - Ackerman's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; 1.2.2 Tree Recursion
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (tree-fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;; Example: counting change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; Exercise 1.11
;;
;; f(n) = { n;                                n < 3  }
;;        { f(n - 1) + 2f(n - 2) + 3f(n - 3); n >= 3 }
;;
;; implement this function using iterative recursion

(define (f n)
  ;; recursive implementation of f
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))

(define (f-tail n)
  ;; tail recursive implementation of f
  (define (iter a b c count)
    (if (= count 0)
      a
      (iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (iter 0 1 2 n))
