;http://lisp.test.openjudge.org/
;28-SICP exercise 3.18
;vbcpascal

#lang racket
(require r5rs)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval '

;;code begins here
(define (check-cycle s)
  (define (inner x mem-lst)
    (cond [(not (pair? x)) #f]
          [(memq x mem-lst) #t]
          [else (inner (cdr x) (cons x mem-lst))]))
  (inner s '()))
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
