;http://lisp.test.openjudge.org/
;33-partial-sums-ex
;vbcpascal

#lang racket
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
         
(eval '(define (stream-ref s n)  ;get the nth item from s. n starts from 
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)

(eval '(define (display-stream s n) ;display first n items of s
  (if (= n 0)
      (displayln "")
      (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))
      env)


(eval '
(define (partial-sums-ex op s)

;;code begins here
  (define (stream-op a b)
    (cons-stream (op (stream-car a) (stream-car b))
                 (stream-op (stream-cdr a)
                            (stream-cdr b))))
  (define (stream-cadr x)
    (stream-car (stream-cdr x)))
  (define (stream-cddr x)
    (stream-cdr (stream-cdr x)))
  (cons-stream (stream-car s)
               (cons-stream (stream-cadr s)
                            (stream-op (stream-cddr s)
                                       (partial-sums-ex op s)))))
                                        
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
