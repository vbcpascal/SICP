;http://lisp.test.openjudge.org/
;30-sum of interval
;vbcpascal

#lang racket
(require r5rs)
(define (to0 x)
  (if (< x 0)
      0
      x))
(define (change-num l r x lst)
  (if (null? (car lst))
      (list (list l r x))
      (cons (list l r x) lst)))
(define (get-sum l r lst)
  (if (null? lst)
      0
      (let ([ln (caar lst)]
            [rn (cadar lst)]
            [xn (caddar lst)])
        (+ (get-sum l r (cdr lst))
           (* xn
              (to0 (+ 1 (- (min r rn)
                           (max l ln)))))))))
(define add-list (list '() '()))
(define (main)
  (define (loop n)
    (if (= n 0)
        (void)
        (begin
          (let ([op (read)]
                [l (read)]
                [r (read)])
            (if (= op 1)
                (let ([x (read)])
                  (set-cdr! add-list (change-num l r x (cdr add-list))))
                (displayln (get-sum l r (cdr add-list)))))
          (loop (- n 1)))))
  (let ([n (read)])
    (loop n)))
(main) 
