;http://lisp.test.openjudge.org/
;03-super-fibonacci-number
;vbcpascal

#lang racket
(define (get-num i a b c d e)
  (if (= i 4)
      e
      (get-num (- i 1)
               b c d e (+ e (* d 4) (* c 5) (* -2 b b) (* a a a)))))
(define (main)
  (let ([a (read)])
    (if (eq? a eof)
        (void)
        (begin
          (if (< a 5)
             (displayln "1")
             (displayln (get-num a 1 1 1 1 1)))
          (main)))))
(main)
