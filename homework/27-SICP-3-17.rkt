;http://lisp.test.openjudge.org/
;27-SICP exercise 3.17
;vbcpascal

#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '

;;code begins here
 (define (count-pairs x)
   (define (inner x mem-lst)
     (if (and (pair? x)
              (not (memq x mem-lst)))
         (inner (car x)
                (inner (cdr x) (cons x mem-lst)))
         mem-lst))
   (length (inner x '())))
;;code ends here

env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)
