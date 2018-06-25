;http://lisp.test.openjudge.org/
;19-sicp exercise 2.58-symbol deriv
;vbcpascal

#lang racket

;;code begins here
(define (=number? a b)
  (and (number? a) (= a b)))
(define (variable? exp) (symbol? exp))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a b)
  (define (to-list x)
    (if (pair? x) x (list x)))
  (cond [(=number? a 0) b]
        [(=number? b 0) a]
        [(and (number? a) (number? b) (+ a b))]
        [else (append (to-list a) '(+) (to-list b))]))
(define (make-product a b)
  (cond [(or (=number? a 0) (=number? b 0)) 0]
        [(=number? a 1) b]
        [(=number? b 1) a]
        [(and (number? a) (number? b) (* a b))]
        [else (list a '* b)]))

(define (sum? x)
  (define (find-plus exp)
    (if (null? exp)
        #f
        (if (eq? (car exp) '+)
            #t
            (find-plus (cdr exp)))))
  (and (pair? x) (find-plus x)))
(define (addend x)
  (define (get-addend a r)
    (if (eq? (car r) '+)
        (if (null? (cdr a)) (car a) a)
        (get-addend (append a (list (car r))) (cdr r))))
  (get-addend '() x))
(define (augend x)
  (define (get-augend r)
    (if (eq? (car r) '+)
        (let ([tmp (cdr r)])
          (if (null? (cdr tmp))
              (car tmp)
              tmp))
        (get-augend (cdr r))))
  (get-augend x))

(define (product? x)
  (cond [(not (pair? x)) #f]
        [(null? (cdr x)) #f]
        [(eq? (cadr x) '*) #t]
        [else #f]))
(define (multiplier x) (car x))
(define (multiplicand x)
  (let ([tmp (cddr x)])
    (if (null? (cdr tmp))
        (car tmp)
        tmp)))
;;code ends here

(define (deriv exp var)
  (cond ((number? exp ) 0)
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
        (else 
         (error "wrong format"))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (deriv a 'x)) (newline) (myloop)))))

(myloop)
