;http://lisp.test.openjudge.org/
;25-BST
;vbcpascal

#lang racket
(define (pd lst)
  (define (pd-a x l t)
    (if (null? l)
        #t
        (let ([y (car l)])
          (cond [t (if (< y x)
                       #f
                       (pd-a x (cdr l) #t))]
                [else
                 (if (> y x)
                     (pd-a x (cdr l) #t)
                     (pd-a x (cdr l) #f))]))))
  (if (null? lst)
      #t
      (if (pd-a (car lst) (cdr lst) #f)
          (pd (cdr lst))
          #f)))
(define (prt lst)
  (if (pd lst)
      (displayln "YES")
      (displayln "NO")))
(let ([a (read)]) (void))
(define (main)
  (define (hhh m lst)
    (if (= m 0)
        lst
        (let ([x (read)])
          (hhh (- m 1) (append lst (list x))))))
  (let ([n (read)])
    (if (eq? n eof)
        (void)
        (begin
          (prt (hhh n '()))
          (main)))))
(main)
