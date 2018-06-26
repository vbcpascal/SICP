;http://lisp.test.openjudge.org/
;32-partial-sums
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
         
(eval '(define (stream-ref s n)  ;取 stream里面第 n 项,n从0开始算
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)


(eval '
 (define (partial-sums op s) ;以s为参数,返回的流是 s0,(op s0 s1),(op s0 s1 s2), ....

;;code begins here
   (define (stream-op a b)
     (cons-stream (op (stream-car a) (stream-car b))
                  (stream-op (stream-cdr a)
                             (stream-cdr b))))
   (cons-stream (stream-car s)
                (stream-op (partial-sums op s)
                           (stream-cdr s))))

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
