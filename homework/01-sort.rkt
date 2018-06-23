;qsort

#lang racket
(define (qsort lst)
  (if (null? lst)
      '()
      (let ([n (car lst)])
        (define (my-loop rest-lst less-n more-n)
          (if (null? rest-lst)
              (append (append (qsort less-n)
                              (list n))
                      (qsort more-n))
              (begin
                (let ([x (car rest-lst)])
                  (cond [(> x n)
                         (my-loop (cdr rest-lst) less-n (cons x more-n))]
                        [(< x n)
                         (my-loop (cdr rest-lst) (cons x less-n) more-n)]
                        [else
                         (my-loop (cdr rest-lst) less-n more-n)])))))
        (my-loop (cdr lst) '() '()))))

(define (read-list)
  (define (read-input-iter lst)
    (let ((a (read)))
      (if (eq? a eof)
          lst
          (read-input-iter (cons a lst)))))
  (read-input-iter '()))

(define (print-list lst)
  (define (print-recur lst)
    (if (null? lst)
        (void)
        (begin (display " ") (display (car lst)) (print-recur (cdr lst)))))
  (begin (display (car lst)) (print-recur (cdr lst))))

(print-list (qsort (read-list)))
