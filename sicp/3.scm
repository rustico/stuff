;1 (libro)
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

(new-withdraw 25)
(new-withdraw 25)
(new-withdraw 65)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request " m))))
  dispatch)

(define W1 (make-account 100))
(define (deposit amount) ((W1 'deposit) amount))
(define (withdraw amount) ((W1 'withdraw) amount))
(deposit 30)
(withdraw 10)

;3.1
(define (make-accumulator initial)
  (lambda (n) 
    (set! initial (+ n initial))
    initial))

(define A (make-accumulator 5))
(A 10)
(A 10)

;3.2 
(define (make-monitored proc)
  (let ((acc 0))
    (lambda (args)
      (if (eq? args 'how-many-calls?)
          acc
          (begin (set! acc (+ 1 acc))
                 (proc args))))))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)

;3.3 3.4
(define (make-account balance stored-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define dispatch 
    (let ((tries 0))
      (lambda (password m)
        (if (equal? stored-password password)
            (begin 
              (set! tries 0)
              (cond ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    (else (error "Unknown request " m))))
            (if (< tries 3)
              (lambda (n) 
                (begin (set! tries (+ tries 1))
                      tries
                      ))
              (lambda (n) "Calling the cops"))))))
  dispatch)

(define acc (make-account 100 '1234))
((acc '1234 'withdraw) 40)
((acc '12345 'deposit) 50)

;3.7
(define (make-joint acc acc-password joint-password)
  (define dispatch
    (lambda (proc password)
        (if (equal? joint-password password)
            (acc acc-password proc)
            (lambda (n) "Password incorrect"))))
  dispatch)

(define acc (make-account 100 '1234))
(define acc-2 (make-joint acc '1234 '5678))
((acc '1234 'withdraw) 40)
((acc-2 'withdraw '5678) 40)

;

(define x '((a b) c d))
(define y '(e f))
(set-car! x y)
(set-car! y 'c) ; cambio el valor en ambas listas

(define x '((a b) c d))
(define y '(e f))
(set-cdr! x y)
(define y (append y '(g h))) ; crea una nueva variable y que no es la apuntada por x
(set-cdr! y (append (cdr y) '(g h))) ; se agregan tambien al final de x

;3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(define w (append! x y))

;3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)) 

x ; bucle infinito  

(define x '(1 2))
(make-cycle x)
(equal? (car x) (caddr x)) ; #t

;3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(mystery '(1 2 3))

;
(define x (list 'a 'b))
(define z1 (cons x x)) ; car y cadr apuntan al mismo objeto x
(eq? (car z1) (cdr z1))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(eq? (car z2) (cdr z2))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)
(set-to-wow! z2)

;3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs '(1 2 (3 4)))
(count-pairs '(1 2 (3 4) (5 6)))

;3.17
(define (in-list? x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) #t)
        (else (in-list? x (cdr l)))))

(in-list? 3 '(1 2 3 4))
(in-list? 3 '(3 2 1 4))
(in-list? 3 '(1 3 2 4))
(in-list? 3 '(1 4 2 3))
(in-list? 5 '(1 2 3 4))

(cadr '(1 (3 4)))
(pair? (cadr '(1 2 (3 4))))

(define (count-pairs l)
  (define (iter l pairs count)
      (if (null? l)
          count
          (if (in-list? (car l) pairs)
              (iter (cdr l) pairs count)
              (iter (cdr l) (cons (car l) pairs) (+ 1 count)))))
  (iter l '() 0))
                     
(count-pairs '(1 2 3))
(count-pairs '(1 2 3 3 3))
(count-pairs '(1 2 (3 4)))
(count-pairs '(1 2 (3 4) (5 6)))
(count-pairs '(1 2 (3 4) (5 6) (5 6)))

;3.18, 3.19
(define x '(1 2 3))
(define y (cons x '()))
(set-car! y '2) ; x no cambia y perdes la referencia
(set-car! x '7) ; y cambia
(set-car! (cdr x) '6) 


(define x '(1 2 3))
(define tmp (car x))
(set-car! x 7)
(set-car! x tmp)

(define (cycle? l)
  (let ((y (cons l l)))
    (define (vueltas x)
      (if (null? x)
          (begin (display x)
          #f)
          (let ((tmp (car x)))
               (display "--------------------")
               (newline)
               (display "X Antes: ")
               (display (car x))

               (set-car! x 'nico)

               (newline)
               (display "X: ")
               (display (car x))
               (newline)
               (display "Y: ")
               (display (cadr y))
               (newline)
                (if (eq? (cadr y) 'nico)
                      (begin (set-car! x tmp)
                              #t)
                      (begin (set-car! x tmp)
                             (display "X Despues: ")
                             (display (car x))
                             (newline)
                             (vueltas (cdr x)))))))
    (vueltas (cdr l))))

(define x '(1 2 3 4))
(make-cycle x)
(cycle? '(1 2 3 4))
(cycle? x)

;http://community.schemewiki.org/?sicp-ex-3.18
(define (cycle? x) 
  (define visited '()) 
  (define (iter x) 
    (set! visited (cons x visited))  ;Va generando una lista de punteros
    (cond ((null? (cdr x)) false) 
          ((in-list? (cdr x) visited) true)  ; cambie memq por in-list?
          (else (iter (cdr x))))) 
  (iter x)) 


(define x '(1 2 3 3 4 1))
(make-cycle x)
(cycle? x)

(define x '(1 2 3 4 5 6 7 8)) 
(define y '(1 2 3 4 5 6 7 8)) 
(set-cdr! (cdddr (cddddr y)) (cdddr y)) 
(define z '(1)) 
(set-cdr! z z) 

(cycle? x)
(cycle? y) ; Rompe mi algoritmo porque es cirular en el medio. Nunca llega al primer elemento
(cycle? z)

; Queues
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Empty queue")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)))))

(define q (make-queue))
(insert-queue! q 1)
(insert-queue! q 2)
(insert-queue! q 3)

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "Empty queue")
      (set-front-ptr! queue (cdr (front-ptr queue)))))

(delete-queue! q)
(front-queue q)

;3.21
(define (print-queue queue)
  (front-ptr queue))

;3.22
(define (make-queue)
  (let ((front-ptr '()) (rear-ptr '()))
      (define (empty?) (null? front-ptr))

      (define (insert! item)
        (let ((new-pair (cons item '())))
          (cond ((empty?) (set! front-ptr new-pair)
                          (set! rear-ptr new-pair))
                (else
                  (set-cdr! rear-ptr new-pair)
                  (set! rear-ptr new-pair)))))

      (define (delete!)
        (if (empty?)
            (error "Empty queue")
            (set! front-ptr (cdr front-ptr))))

      (define (dispatch m)
        (cond ((eq? m 'empty?) (empty?))
              ((eq? m 'insert!) insert!)
              ((eq? m 'print) front-ptr)
              ((eq? m 'rear) rear-ptr)
              ((eq? m 'delete!) (delete!))
              (else (error "Error"))))
      dispatch))

(define q (make-queue))
((q 'insert!) 1)
((q 'insert!) 2)
((q 'insert!) 3)

(q 'delete!)
(q 'print)
(q 'empty?)
((q 'insert!) 4)
(q 'print)

;; deque
(define (make-deque)
  (let ((front-ptr '()) 
	(rear-ptr '()))
      (define (empty?) (null? front-ptr))

      (define (front-insert! item)
        (let ((new-pair (cons item '())))
          (cond ((empty?) (set! front-ptr new-pair)
                          (set! rear-ptr new-pair))
                (else
                  (set-cdr! new-pair front-ptr)
                  (set! front-ptr new-pair)))))

      (define (rear-insert! item)
        (let ((new-pair (cons item '())))
          (cond ((empty?) (set! front-ptr new-pair)
                          (set! rear-ptr new-pair))
                (else
                  (set-cdr! rear-ptr new-pair)
                  (set! rear-ptr new-pair)))))

      (define (delete-front!)
        (if (empty?)
            (error "Empty queue")
            (set! front-ptr (cdr front-ptr))))

      (define (element-to-rear front-ptr)
	(define (iter l)
	  (cond ((null? l) '())
		((null? (cdr l)) l)
		((null? (cddr l)) l)
		(else (iter (cdr l)))
	))
	(iter front-ptr))

      (define (delete-rear!)
	(if (empty?)
	    (error "Empty queue")
	    (let ((ptr-to-rear (element-to-rear front-ptr)))
		  (set-cdr! ptr-to-rear '()))
	  ))

      (define (dispatch m)
        (cond ((eq? m 'empty?) (empty?))
              ((eq? m 'insert-front!) front-insert!)
	      ((eq? m 'insert-rear!) rear-insert!)
              ((eq? m 'front) front-ptr)
              ((eq? m 'rear) rear-ptr)
              ((eq? m 'delete-front!) (delete-front!))
              ((eq? m 'delete-rear!) (delete-rear!))
              (else (error "Error"))))
      dispatch))

(define dq (make-deque))
(define dq-insert-front (dq 'insert-front!))
(define dq-insert-rear (dq 'insert-rear!))

(dq-insert-front 1)
(dq-insert-front 2)
(dq-insert-front 3)
(dq-insert-front 4)
(dq-insert-rear 1)
(dq-insert-rear 2)
(dq-insert-rear 3)
(dq-insert-rear 4)


; Tables
; 1D
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define t (make-table))
(insert! 'uno 1 t)
(lookup 'uno t)

; 2D
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
            (cdr record)
            false))
      false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
            (if record
                (set-cdr! record value)
                (set-cdr! subtable
                          (cons (cons key-2 value)
                                (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;3.25
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter k table)
        (if (null? k)
            (cdr table)
            (let ((record (assoc (car k) (cdr table))))
              (if record
                (begin 
                (iter (cdr k) record))
                #f ))))
      (iter keys local-table))

    (define (insert! keys value)
      (define (iter keys table)
        (let ((st (assoc (car keys) (cdr table))))
          (if st
              (if (null? (cdr keys))
                  (set-cdr! st value)
                  (iter (cdr keys) st))
              (if (null? (cdr keys))
                  (set-cdr! table
                            (cons (cons (car keys) value)
                                  (cdr table)))
                  (begin
                    (set-cdr! table (cons (cons (car keys) '()) (cdr table)))
                    (iter (cdr keys) (cadr table)))))))
      (iter keys local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'show) (lambda () (display local-table)))
            (else (error "Error"))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))
(define show (operation-table 'show))

(put '(dos) 2)
(get '(dos))
(put '(dos) 5)
(get '(dos))

(put '(numeros dos) 2)
(put '(numeros tres) 3)
(get '(numeros dos))
(get '(numeros tres))


(put '(numeros dos) 3)
(put '(numeros cuatro) 4)
(get '(numeros tres))
(get '(numeros dos))
(get '(numeros cuatro))

(put '(letras vocales a) 'a)
(get '(letras vocales a))

;3.26
(define (binary-tree root)
  (list root '() '())
)

(define (left-node node)
  (cadr node)
)

(define (right-node node)
  (caddr node)
)

(define (key-node node)
  (car node)
)

(define (set-left-node parent node)
  (set-car! (cdr parent) node)
)

(define (set-right-node parent node)
  (set-car! (cddr parent) node)
)

(define (search-parent-node t element)
  (cond ((null? (cdr t)) t)
	((> (key-node t) element)
	 (if (null? (left-node t))
	     t
	     (search-parent-node (left-node t) element)))
	(else
	 (if (null? (right-node t))
	     t
	     (search-parent-node (right-node t) element)))
))

(define (binary-tree-add t element)
  (let ((parent-node (search-parent-node t element))
	(child-node (list element '() '())))
    (if (> (key-node parent-node) element)
	(set-left-node parent-node child-node)
	(set-right-node parent-node child-node))
    ))

(define t '(5 (3 () ()) (8 () ())))
(define t '(5 (3 (1) (4)) (8)))

;3.27
(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((prev (lookup x table)))
        (or prev
            (let ((result (f x)))
              (insert! x result table)
              result))))))

;3.3.4
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not  (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1)) 
      1
      0))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
          
;http://community.schemewiki.org/?sicp-ex-3.29
(define (or-gate a1 a2 output)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (c3 (make-wire)))
    (inverter a1 c1)
    (inverter a2 c2)
    (and-gate c1 c2 c3)
    (inverter c3 output))
  'ok)

;3.30
(define (rc-adder numberA numberB solution c)
  (if (null? numberA)
      solution
      (let ((c-in (make-wire)))
        (full-adder (car numberA) (car numberB) c-in (car solution) c)
        (rc-adder (cdr numberA) (cdr numberB) (cdr solution) c))))
        
;
(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error 'Unknown operation))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
 (set-car! agenda time)) 

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
            (set-cdr!
              segments
              (cons (make-new-time-segment time action)
                    (cdr segments)))
            (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
          agenda
          (cons (make-new-time-segment time action)
                segments))
          (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;3.3.5
(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
        ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
        ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else 
            (error "Unknown request"))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;terminarlo

;3.4
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance balance))
            (else (error "Unknown request " m))))
    dispatch))
