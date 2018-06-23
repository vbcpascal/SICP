;http://lisp.test.openjudge.org/
;14-super-map
;vbcpascal

#lang racket
(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst))
            (map op (cdr lst)))))
  
(define (super-map op . w)

;;code begins here
  (define (work lst lst-res)
    (if (null? (car lst))
        lst-res
        (work (map cdr lst)
              (append lst-res
                      (list (apply op (map car lst)))))))
  (work w '()))
;;code begins here
  
(define (myloop)
  (let ((a (read))
        (b (read))
        (c (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (super-map + a b c)) 
               (displayln (super-map (lambda (x y) (+ x (* 2 y) )) a b ))
               (myloop)))))
(myloop)
