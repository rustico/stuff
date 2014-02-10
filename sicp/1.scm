(+ 2 5)
(+ 
  (* 4 5)
  (* 5 4)
  1)
)

(define size 5)
(- 5 size)

(define radius 10)
(define pi 3.14159)
(define circumference (* 2 pi radius))

circumference

(define (circumferencia x) (* x pi radius))
(circumferencia 2)

(define (square x) (* x x))
(define (sum-of-squares x y)
        (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((>= x 0) x)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (max a b)
        (if (> a b)
            a
            b))

(define (max3 a b c)
    (cond ((and (>= a b) (>= a c)) a)
          ((and (>= b a) (>= b c)) b)
          (else c)))

(define (min a b c)
    (cond ((and (<= a b) (<= a c)) a)
          ((and (<= b a) (<= b c)) b)
          (else c)))


(define (sq3 a b c)
    (cond ((= c (min a b c)) (sum-of-squares a b))
          ((= b (min a b c)) (sum-of-squares a c))
          (else (sum-of-squares a c))))
          

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


(define (new-if p then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x oldguess)
  (if (good-enough2? guess oldguess)
      guess
      (sqrt-iter (improve guess x)
                 x
                 guess)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enough2? guess oldguess)
  (< (abs (-  guess oldguess)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x 0.0))

(define (sqrt2 x)
    (define (improve guess)
        (average guess (/ x guess)))

    (define (good-enough2? guess oldguess)
        (< (abs (-  guess oldguess)) 0.001))

    (define (sqrt-iter guess oldguess)
        (if (good-enough2? guess oldguess)
            guess
            (sqrt-iter (improve guess)
                        guess)))
  (sqrt-iter 1.0 0.0))

(define (count-change amount)
  (cc amount 3))

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



(define (even? n)
  (= (remainder n 2) 0))

(define (double a)
  (+ a a))

(define (halve a)
  (if (even? a)
      (/ a 2)
      a))

(define (f* b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n ) (iter a (double b) (halve n)))
          (else (iter (+ a b) b (- n 1)))))
  (iter 0 b n))


(define (next n)
  (if (= n 2)
      3
      (+ 2 n)))
       
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Calculates the exponential of a number modulo another number
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
         (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
    (report-time (- (runtime) start-time))
    #f))

(define (report-time elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)


(define (search-for-primes n)
  (define (iter n primes)
    (cond ((and (not (even? n)) (timed-prime-test n)) 
           (iter  (+ 1 n) (- primes 1)))
          ((> primes 0)
            (iter  (+ 1 n) primes))))
  (iter (+ 1 n) 3))

(define (sum term a next b)
  (if (> a b) 
      0
      (+  (term a)
        (sum term (next a) next b))))

(define (inc n) (+ 1 n))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (filtered-accumulate filter? combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter? a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (sum-of-prime-squares a b) (filtered-accumulate prime? + 0 square a inc b)) 

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+  (* x (square a))
        (* y b)
        (* a b))))

(define plus4 (lambda (x) (+ x 4)))

(define (hola nombre)
  (define saludo "saludo")
  (display "Hola ")
  (display saludo)
  (display nombre))

(define (saludo msj)
  (lambda (x) (display (string-append msj " " x))))

(define hola (saludo "hola"))
(define bonjour (saludo "bonjour"))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter g n)
    (if (= n 1)
        g
        (iter (compose f g) (- n 1))))
  (iter f n))

((repeated square 1) 5)
((repeated square 2) 5)
