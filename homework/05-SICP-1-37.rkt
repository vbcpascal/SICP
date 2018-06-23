;http://lisp.test.openjudge.org/
;05-SICP exercise 1.37
;vbcpascal

#lang racket
(define (cont-frac-iter N D k)
;;code begin here

  (define (iter curr result)
    (if (= curr 0)
        result
        (iter (- curr 1) (/ (N curr) (+ (D curr) result)))))
  (iter k 0))

;;code end here
(cont-frac-iter (lambda (x) x) 
           (lambda (x) 1.0)
           30)
 
(cont-frac-iter (lambda (x) (* 2 x))
           (lambda (x) (* 1.0 x))
           30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)
