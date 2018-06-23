;http://lisp.test.openjudge.org/
;08 sicp exercise 1.46
;vbcpascal

#lang racket

(define (average x y) (/ (+ x y) 2))

(define (iterative-improve good-enough improve)

;;code begins here
  (lambda (x)
    (if (good-enough x)
        x
        ((iterative-improve good-enough improve) (improve x)))))
;;code ends here

(define (sqrt a)
  (define (gd x)
    (< (abs (- x (average x (/ a x)))) 0.0001))
  (define (im x)
    (average x (/ a x)))
  ((iterative-improve gd im) 1))

(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (display (sqrt n)) 
               (newline) (myloop)))))

(myloop)
