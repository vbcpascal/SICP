;http://lisp.test.openjudge.org/
;12-deep-reverse-a-tree
;vbcpascal

#lang racket
(define (deep-reverse olst)
  (if (null? olst)
      '()
      (let ([x (car olst)])
        (if (list? x)
            (append (deep-reverse (cdr olst)) (list (deep-reverse x)))
            (append (deep-reverse (cdr olst)) (list x))))))
(define (main)
  (let ([a (read)])
    (if (eq? a eof)
        (void)
        (begin
          (displayln (deep-reverse a))
          (main)))))
(main)
