;http://lisp.test.openjudge.org/
;03-fast-power
;vbcpascal

#lang racket
(define (pow a n)
  (define (fast-p a n tmp)
    (if (= n 1)
        (* a tmp)
        (if (even? n)
            (fast-p (* a a) (/ n 2) tmp)
            (fast-p (* a a) (/ (- n 1) 2) (* tmp a)))))
  (fast-p a n 1))
(define (main)
  (let ([a (read)]
        [b (read)])
    (if (eq? a eof)
        (void)
        (begin
          (displayln (pow a b))
          (main)))))
(main)
