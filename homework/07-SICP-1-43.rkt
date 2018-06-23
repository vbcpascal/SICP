;http://lisp.test.openjudge.org/
;sicp exercise 1.43
;vbcpascal

#lang racket
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (db x) (* x 2))

(define (repeated f n)

;;code begins here
  (define (work x k)
    (if (= k 0)
        x
        (work (f x) (- k 1))))
  (lambda (x) (work x n)))
;;code ends here

((repeated square 2) 5)
((repeated inc 4) 6)
((repeated db 4) 6)

(display "********") (newline)

(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (display ((repeated square n) 2)) 
               (newline) (myloop)))))

(myloop)
