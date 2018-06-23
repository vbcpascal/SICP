;http://lisp.test.openjudge.org/
;09 sicp exercise 2.5
;vbcpascal

#lang racket

;;code begins here
(define (get-num n x tmp)
  (if (= (remainder n x) 0)
      (get-num (/ n x) x (+ tmp 1))
      tmp))
(define (car n) (get-num n 2 0))
(define (cdr n) (get-num n 3 0))
;;code ends here

(define (fast-exp a n)
  (define (square x) (* x x))
  (define (iter a n result)
    (if (= n 0)
        result
        (if (even? n) 
            (iter (square a) (/ n 2) result)
            (iter (square a) (/ (- n 1) 2) (* a result)))))
  (iter a n 1))
  
(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (display (car (cons a b)))
               (display " ")
               (display (cdr (cons a b)))
               (newline) 
               (myloop)))))

(myloop)
