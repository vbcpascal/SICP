;http://lisp.test.openjudge.org/
;10-reverse a list
;vbcpascal

#lang racket
(define (my-reverse lst)
  (define (work olst nlst)
    (if (null? olst)
        nlst
        (work (cdr olst)
              (cons (car olst) nlst))))
  (work lst '()))

(define (main)
  (let ([a (read)])
    (if (eq? a eof)
        (void)
        (begin
          (displayln (my-reverse a))
          ;actually can use #<procedure:reverse> in racket
          (main)))))
(main)
