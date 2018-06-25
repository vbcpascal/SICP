;http://lisp.test.openjudge.org/
;21-relative complement & symmertric difference
;vbcpascal

#lang racket
(define (remove-rep lst)
  (cond [(null? lst) lst]
        [(null? (cdr lst)) lst]
        [(= (car lst) (cadr lst)) (remove-rep (cdr lst))]
        [else (cons (car lst) (remove-rep (cdr lst)))]))

(define (union-set a b)
  (cond [(null? a) b]
        [(null? b) a]
        [else
         (let ([a-f (car a)]
               [b-f (car b)])
           (cond [(> a-f b-f) (cons b-f (union-set a (cdr b)))]
                 [(< a-f b-f) (cons a-f (union-set (cdr a) b))]
                 [(= a-f b-f) (cons a-f (union-set (cdr a) (cdr b)))]))]))

(define (relative-set a b)
  (cond [(null? a) '()]
        [(null? b) a]
        [else
         (let ([a-f (car a)]
               [b-f (car b)])
           (cond [(> a-f b-f) (relative-set a (cdr b))]
                 [(< a-f b-f) (cons a-f (relative-set (cdr a) b))]
                 [(= a-f b-f) (relative-set (cdr a) (cdr b))]))]))

(define (symmetric-set a b)
  (union-set (relative-set a b) (relative-set b a)))

(define (myloop)
  (let ([a (read)]
        [b (read)])
    (if (eq? a eof)
        (void)
        (let ([a-ordered (remove-rep (sort a <))]
              [b-ordered (remove-rep (sort b <))])
          (begin
            (display (relative-set a-ordered b-ordered))
            (displayln (symmetric-set a-ordered b-ordered))
            (myloop))))))

(myloop)
                    
    
