;http://lisp.test.openjudge.org/
;02-pascal-triangle
;vbcpascal

#lang racket

(define (list-add a b lst)
  (if (null? a)
      lst
      (list-add (cdr a)
                (cdr b)
                (append lst (list (+ (car a) (car b)))))))
(define (prt lst)
  (if (null? lst)
      (newline)
      (begin
        (display (car lst))
        (display " ")
        (prt (cdr lst)))))
(define (print-all n)
  (define (get-one i lst)
    (let ([new-lst (list)])
      (if (= i 1)
          (set! new-lst '(1))
          (set! new-lst (list-add (append lst (list 0))
                                  (append (list 0) lst)
                                  '())))
      (prt new-lst)
      (if (> i n)
          (void)
          (get-one (+ i 1) new-lst))))
  (get-one 1 '()))
(define (main)
  (let ([a (read)])
    (if (eq? a eof)
        (void)
        (begin
           (print-all (- a 1))
           (main)))))
(main)
