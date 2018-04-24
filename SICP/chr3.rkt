#lang racket

(define (square n)
  (* n n))

(define (gcd a b)
  (if (= 0 b) a
      (gcd b (remainder a b))))

;EG 3.1.1.1 - bank account
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
    (cond [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request -- MAKE-ACCOUNT" m)]))
  dispatch)
;(define acc (make-account 100))
;((acc 'withdraw) 50)
;((acc 'deposit) 60)
;((acc 'withdraw) 200)

;EX 3.1 - make-accumulator
(define (make-accumulator balance)
  (define (add x)
    (begin
      (set! balance (+ x balance))
      balance))
  add)
;(define A (make-accumulator 5))
;(A 5)
;(A 6)

;EX 3.2 - monitored version of sqrt
(define (make-monitored f)
  (let ([call-count 0])
    (define (calculate x)
      (begin
        (set! call-count (+ 1 call-count))
        (f x)))
    (define (ask-count)
      call-count)
    (define (reset-count)
      (set! call-count 0))
    (define (dispatch m)
      (cond [(eq? m 'how-many-calls) (ask-count)]
            [(eq? m 'reset-count) (reset-count)]
            [else (calculate m)]))
    dispatch))
;(define s (make-monitored sqrt))
;(s 100)
;(s 20)
;(s 'how-many-calls)

;EX 3.3 - account with password
(define (make-account-safe balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m p)
    (cond [(not (eq? p password)) (lambda (x) (displayln "Incorrect password"))]
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request -- MAKE-ACCOUNT" m)]))
  dispatch)
;(define acc (make-account-safe 100 'drracket))
;((acc 'withdraw 'drracket) 20)
;((acc 'withdraw 'trracket) 10)

;EG 3.1.2.1 -
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials ceraso-test))))
(define (ceraso-test)
  (= (gcd (random 1000000) (random 1000000)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (begin
      ;(if (= trials-passed 0)
      ;    (displayln "#0.00#")
      ;    (displayln (sqrt (/ 6 (* (/ trials-passed (- trials trials-remaining)) 1.0)))))
      (cond [(= trials-remaining 0)
             (/ trials-passed trials)]
            [(experiment)
             (iter (- trials-remaining 1) (+ trials-passed 1))]
            [else
             (iter (- trials-remaining 1) trials-passed)])))
  (iter trials 0))
;(estimate-pi 10000) 

;EX 3.5
(define (estimate-f lower-x upper-x lower-y upper-y f trials)
  (define (get-rand lower upper)
    (+ lower (* (random) (- upper lower))))
  (define (experiment)
    (let ([x (get-rand lower-x upper-x)]
          [y (get-rand lower-y upper-y)])
      (f x y)))
  (* 4.0 (monte-carlo trials experiment)))
(define (f-3-5 x y)
  (<= (+ (* x x) (* y y)) 1))
;(estimate-f -1 1 -1 1 f-3-5 10000)

;EX 3.7
(define (make-joint old-account old-password my-password)
  (lambda (password command)
    (if (eq? password my-password)
        (old-account command old-password)
        (lambda(x) (displayln "Incorrect Password - New")))))
;(define accc (make-joint acc 'drracket 'a))
;((accc 'a 'withdraw) 20)

;EX 3.8
(define f-3-8-mine
  (let ([val 0])
    (lambda (x)
      (let ([old-val val])
        (begin
          (when (> x val) (set! val x))
          old-val)))))
;(+ (f 1) (f 0))

(define f-3-8-key
    (lambda (first-value)
        (set! f-3-8-key (lambda (second-value) 0))
        first-value))
;(+ (f-3-8-key 1) (f-3-8-key 0)) 




