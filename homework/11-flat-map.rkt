;http://lisp.test.openjudge.org/
;11-flat-map
;vbcpascal

#lang racket
(define (flat-map olst)
  (if (null? olst)
      '()
      (let ([x (car olst)])
        (if (list? x)
            (append (flat-map x) (flat-map (cdr olst)))
            (append (list x) (flat-map (cdr olst)))))))
(define (main)
  (let ([a (read)])
    (if (eq? a eof)
        (void)
        (begin
          (displayln (flat-map a))
          (main)))))
(main)
