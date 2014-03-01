(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons y x))

(car (car z))
(car (cdr z))

(define (positive? n)
  (>= n 0))

(define (positive? n)
  (< n 0))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 (- 2)))

(define (sign n d)
  (if (or (and (positive? n) (positive? d)) (and (negative? n) (negative? d)))
      1
      (- 1)))

(define (abs x)
  (cond ((>= x 0) x)
        ((< x 0) (- x))))

(define (make-rat n d) 
  (let ((s (sign n d)))
    (cons (* s (abs n)) (abs d))))

(make-rat 2 3)
(make-rat 2 (- 3))
(make-rat (- 2) (- 3))
(make-rat (- 2) 3)

(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p)))

(define (cdr2 z)
  (z (lambda (p q) q)))


(define hola (cons2 1 2))
(hola (lambda (a b) (+ a b) ))

(define zero (lambda (f) (lambda (x) x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound y) (lower-bound x))
                 (- (upper-bound y) (upper-bound x))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (lower-bound y)))
        (p3 (* (lower-bound x) (lower-bound y)))
        (p4 (* (lower-bound x) (lower-bound y))))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (= (upper-bound y) 0) 
          (= (lower-bound y) 0))
      (error "Division by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                  (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (upper-bound interval) 
  (car interval))

(define (upper-bound interval) 
  (cdr interval))

(define (lower-bound interval) 
  (car interval))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define interval-1 (make-interval 1 21))
(width interval-1)
(define interval-2 (make-interval 4 7))
(width interval-2)
(define interval-sum (add-interval interval-1 interval-2))
(define interval-sub (sub-interval interval-1 interval-2))
(width interval-sum)
(width interval-sub)
(= (width interval-sum) (+ (width interval-1) (width interval-2)))
(= (width interval-sub) (- (width interval-1) (width interval-2)))

(define interval-3 (make-interval 8 16))
(define interval-4 (make-interval 2 4))
(div-interval interval-3 interval-4)

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (center-percent c p)
  (* (/ 1.0 p) c ))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (let ((w (center-percent c p)))
       (make-center-width c w))) 

 (define (make-interval-center-percent c pct) 
   (let ((width (* c (/ pct 100.0)))) 
     (make-interval (- c width) (+ c width)))) 

(make-center-percent (center interval-1) 10)
(make-interval-center-percent (center interval-1) 10)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-center-percent (center interval-1) 05))
(define r2 (make-center-percent (center interval-2) 05))
(par1 r1 r2)
(par2 r1 r2)


(define r1 (make-center-percent (center interval-1) 90))
(define r2 (make-center-percent (center interval-2) 90))
(par1 r1 r2)
(par2 r1 r2)

(define numeros (list 1 2 3 4 5))
(car numeros)
(cadr numeros)
(caddr numeros)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

(define (length lista)
  (define (iter l n)
    (if (null? l)
        n
        (iter (cdr l) (+ n 1))))
  (iter lista 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define l1 (list 1 2 3))
(define l2 (list 4 5 6))
(append l1 l2)

(cons 1 (cons 2 (cons 3 l2))))

(define l1 (cons 1 (cons 2 '())))
(define l1-reverse (cons (cddr l1) (cons (cdr l1) (cons (car l1) '()))))
(null? (car l1-reverse))

(define (reverse l)
  (define (iter l r)
    (if (null? l)
        r
        (iter (cdr l) (cons (car l) r))))
  (iter (cdr l) (cons (car l) '() )))


(define (even? x)
  (= (remainder x 2) 0))

(define (odd? x)
  (not (even? x)))

; Lo devuelve al reves
(define (same-parity x . l)
  (define (iter l-tmp r)
      (if (null? l-tmp)
          r
          (if (or (and (even? x) (even? (car l-tmp)))
                  (and (odd? x) (odd? (car l-tmp))))
              (iter (cdr l-tmp) (cons (car l-tmp) r))
              (iter (cdr l-tmp) r)
              
                )))
  (iter l '() ))

(same-parity 1 2 3 4 5 6 7 8 9 10 11 12)

(define (same-parity x . l)
  (define (tmp l)
    (if (null? l)
        '()
        (if (or (and (even? x) (even? (car l)))
                (and (odd? x) (odd? (car l))))
            (cons (car l) (tmp (cdr l)))
            (tmp (cdr l)))))
  (tmp l))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
          (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))


(define (even-fibs n)
  (define (next k)
    (if (> k n)
      '()
      (let  ((f (fib k)))
        (if (even?  f)
            (cons f (next (+  k 1)))
            (next (+ k 1))))))
  (next 0))
  
(define (range low high)
  (if (> low high)
      '()
      (cons low (range (+ 1 low) high))))

(range 0 20)

(define (enumerate-tree tree)
  (if (null? tree)
      '()
      (if (pair? tree)
          (cons (enumerate-tree (car tree)) (enumerate-tree (cdr tree)))
          tree)))

(enumerate-tree (list 1 2))
(enumerate-tree (list 1 2 3))
(define lista (cons 1 (cons 2 (cons 3 '()))))
(enumerate-tree lista )

(define (enumerate-tree tree)
  (if (null? tree)
      '()
      (if (pair? tree)
          (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree)))
          (list tree))))


(define lista (list 1 (list 2 3)))
(enumerate-tree lista )

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) (map p (cdr sequence)))) '() sequence))

(map (lambda (x) (* x 2)) (list 1 2 3))

(define (append s1 s2)
  (accumulate cons s2 s1))

(append (list 1 2) (list 3 4))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 (length (cdr sequence)))) 0 sequence))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3 4))
(length (list 1 2 3 4 6 7))
(length (list 1 ))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(map car seqs)
(map cdr seqs)

(define (accumulate-n op init seqs)
  (if (null?  (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 seqs)
(accumulate-n * 1 seqs)

(accumulate + 0 (accumulate-n * 1 seqs))

(define m1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define m2 (list (list 11 12 13) (list 14 15 16) (list 17 18 19)))
(define column (map car m2))
(define row (car m1))
(list row column)

(accumulate-n * 1 (list row column))

; Devuelve la primer fila
(define (n-row m1 m2)
  (if (null? (car m2))
      '()
      (let ((row (car m1))
            (column (map car m2)))
        (cons (accumulate +  0 (accumulate-n * 1 (list row column)))
              (n-row m1 (map cdr m2))))))

(n-row m1 m2)

; Hace el producto de dos matrices no de dos vectores como es en el libro
(define (m*m m1 m2)
  (if (null? m1)
      '()
      (cons (n-row m1 m2) (m*m (cdr m1) m2))))

(m*m m1 m2)

; No funciona con matrices si con vectores, mire mal
(define (dot-product m1 m2)
  (accumulate + 0 (map * m1 m2)))

(dot-product (list 1 2 3) (list 4 5 6))

(define v1 (list 20 21 22))
(accumulate +  0 (accumulate-n * 1 (list (car m1) v1)))

(define (matrix-*-vector m v)
  (map (lambda (s) (accumulate +  0 (accumulate-n * 1 (list s v)))) m ))

(matrix-*-vector m1 v1)

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose m1)

(define (m*m m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                (accumulate +  0 (accumulate-n * 1 (list row col))))
                cols))
         m)))
        
(m*m m1 m2)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

(fold-right + 0 (list 1 2 3))
(fold-left +  0 (list 1 2 3))

; 2.39 Verlo mas tarde
(define (reverse sequence)
  (fold-right (lambda (x y ) 
                 (display x)
                 (display " ")
                 (display y)
                 (newline)
                 (if  (null? y)
                      x
                      (cons y x)))
                '() sequence))

(reverse (list 1 2 3))

(define (reverse sequence)
  (fold-left (lambda (x y ) (cons y x )) '() sequence))

(reverse (list 1 2 3))


(define enumerate-interval range)
(define nil '())

(range 1 0)
(range 1 2)

(accumulate append
            nil
            (map  (lambda (i)
                    (map (lambda (j) (list i j))
                         (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 6)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; Permutando: ni cerca
(define l1 (list 1 2 3 4))
(append (cdr l1) (list (car l1)))
(list l1)
(define ls (list (list 1 2) (list 2 3)))
(append (list l1) ls)
(append (list l1) nil)

(define (p l n)
  (define (iter l-tmp i result)
    (if (= i (length l))
        result
        (iter (append (cdr l-tmp) (list (car l-tmp))) 
              (+ i 1) 
              (append (list (append (list n) l-tmp)) result))))
  (iter l 0 nil))
             
(p (list 1 2 3) 4)

(define (p2 l)
  (define (iter l-tmp i result)
    (if (= i (length l))
      result
      (iter (append (cdr l-tmp) (list (car l-tmp))) 
            (+ i 1) 
            (append (p (cdr l-tmp) (car l-tmp)) result))))
  (iter l 0 nil))

(length (p2 (list 1 2 3)))
(p2 (list 1 2 3 4))
(length (p2 (list 1 2 3 4)))
(length (p2 (list 1 2 3 4 5)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j)) (range (+ i 1) n)))
           (range 1 n)))

(unique-pairs 4)

; Pensarlo
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
            (lambda (positions) (safe? k positions))
            (flatmap
              (lambda (rest-of-queens)
                (map  (lambda (new-row)
                        (adjoin-position new-row
                                         k
                                         rest-of-queens))
                      (range 1 board-size)))
              (queen-cols (- k 1))))))
  (queen-cols board-size))


;2.3

(list 'a 'b)
(define letters '(a b c d e))
(car letters)
(cdr letters)

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item  (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear bannana prune))
(memq 'apple '(pear (apple sauce) bannana apple y prune))
(eq? 1 1)
(eq? 'nico 'nico)

(pair? (car '(a short list)))

(define (equal? a b)
  (cond ((and (not (pair? (car a))) (not (pair? (car b))))
            (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((and (pair? (car a)) (pair? (car b)))
            (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)))

(equal? (list 1 2 3 4) (list 1 2 3 4))
(equal? (list 1 2 3 4) (list 1 2 3))
(equal? (list 1 2 3 4) (list 1 2 4 3))
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(equal? '(this (is a) list) '(this (is a) list))

(car '(+ x 2))
(cdr '(+ x 2))
(pair? (cdr '(+ x 2)))
(cadr '(+ x 2))
(cddr '(+ x 2))


(define variable? symbol?)
(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? e) 
  (and (pair? e) (eq? (car e) '+)))

(define (addend e) 
  (cadr e))

(define (augend e) 
  (caddr e))

(define (augend e) 
  (let ((v (cddr e)))
    (if (null? (cdr v))
        (car v)
        (append '(+) v))))

(define (make-sum a1 a2)
      (list '+ a1 a2))

(make-sum 2 3)

(define (product? e) 
  (and (pair? e) (eq? (car e) '*)))
(product? (list '* 2 3))
(product? (list '+ 2 3))
(sum? (list '+ 2 3))
(sum? (list '* 2 3))

(define (multiplier e) (cadr e))
(define (multiplicand e) (caddr e))


(define (multiplicand e) 
  (let ((v (cddr e)))
    (if (null? (cdr v))
        (car v)
        (append '(*) v))))

(multiplicand (list '* 2 3))
(define (make-product m1 m2)
  (list '* m1 m2))
(make-product 2 3)

(deriv 2 'x)
(deriv 'x 'x)
(deriv 'y 'x)

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(define (make-sum a1 a2)
  (cond ((eq? a1 0) a2)
        ((eq? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))


(define (make-product a1 a2)
  (cond ((or (eq? a1 0) (eq? a2 0)) 0)
        ((eq? a1 1) a2)
        ((eq? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))

(define (make-exponentiation b e)
  (cond ((eq? b 0) 0)
        ((eq? e 1) b)
        ((eq? e 0) 1)
        (else (list '** b e))))

(define e1 (make-exponentiation 2 3))
(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(exponentiation? e1)

(define (base e) (cadr e))
(define (exponent e) (caddr e))
(base e1)
(exponent e1)


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
            (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var)))
        ((product? exp)
            (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp))))
        ((exponentiation? exp)
            (make-product (make-product (exponent exp) 
                                        (make-exponentiation (base exp) (- (exponent exp) 1)))
                          (deriv (base exp) var)))
        (else (error "unknown expression type"))))

(define ex (make-exponentiation 'x 3))
(deriv ex 'x)

(deriv '(* x y (+ x 3)) 'x)
'(+ x 3 (** x 3))
(deriv '(+ x 3 (** x 3)) 'x)

(deriv '(+ x 3 x) 'x)
(deriv '(+ x x 3) 'x)

(deriv '(+ x 3 x) 'x)
(deriv '(* x 3 x) 'x)

(deriv '(* x y (+ x 3)) 'x)
 (deriv '(* (* x y) (+ x 3)) 'x) 


