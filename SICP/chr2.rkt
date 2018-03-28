#lang racket

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ 1 a) b))))

(define (square n)
  (* n n))

(define (gcd a b)
  (if (= 0 b) a
      (gcd b (remainder a b))))

(define (get-log n index)
  (if (= n 1)
      0
      (+ 1 (get-log (/ n index) index))))

;EG 2.1.1.1 - 有理数
;EX 2.1
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))
(define (denom x) (cdr x))
(define (numer x) (car x))
(define (make-rat n d)
  (define (simplify n d)
    (cond ((< d 0) (cons (- 0 n) (- 0 d)))
          (else (cons n d))))
  (let ((g (gcd n d)))
    (simplify (/ n g) (/ d g))))
;(display (make-rat 4 -6))

;EX 2.4
(define (cons-ex2-4 x y)
  (lambda (m) (m x y)))
(define (car-ex2-4 z)
  (z (lambda (p q) p)))
(define (cdr-ex2-4 z)
  (z (lambda (p q) q)))
;(car (cons 2 4)) (cdr (cons 2 4))

;EX 2.5
(define (car-ex2-5 x)
  (if (= (remainder x 3) 0)
      (car-ex2-5 (/ x 3))
      (get-log x 2)))
(define (cdr-ex2-5 x)
  (if (= (remainder x 2) 0)
      (cdr-ex2-5 (/ x 2))
      (get-log x 3)))
(define (cons-ex2-5 a b)
  (* (expt 2 a) (expt 3 b)))
;(car-ex2-5 (cons-ex2-5 3 5))

;EX 2.6
(define zero-ex2-6 (lambda (f) (lambda (x) x)))
(define (add1-ex2-6 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one-ex2-6 (lambda (f) (lambda (x) (f x))))
(define two-ex2-6 (lambda (f) (lambda (x) (f (f x)))))
(define (add-ex2-6 m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x))))) 

;EG 2.1.4.1
(define (make-interval a b) (cons a b))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;EX 2.7
(define (upper-bound z) (cdr z))
(define (lower-bound z) (car z))

;EX 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;EX 2.10
(define (div-interval x y)
  (if (<= (* (upper-bound y) (lower-bound y)) 0)
      (displayln "error")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound x))))))

;EX 2.11
(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (> lx 0) (> ux 0))
           (cond ((and (> ly 0) (> uy 0)) (make-interval (* lx ly) (* ux uy)))
                 ((and (<= ly 0) (>= uy 0)) (make-interval (* lx uy) (* ux ly)))
                 ((and (> ly 0) (> uy 0)) (make-interval (* lx uy) (* ux uy))))))))

;EX 2.12
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-center-percent c p)
  (make-interval (* c (- 1 p)) (* c (+ 1 p))))
(define (percent i)
  (let ((c (center i))
        (w (width i)))
    (/ (* 1.0 w) c)))

;EX 2.13

;EX 2.17
(define (last-pair x)
  (if (= (length x) 1)
      x
      (last-pair (cdr x))))
;(last-pair (list 12 74 89 52 62))

;EX 2.18
(define (reverse-list lst)
  (if (null? lst) (list)
      (append (reverse-list (cdr lst)) (list (car lst)))))
;(reverse-list (list 1 4 9 16 25))

;EX 2.19

;EX 2.20
(define (same-parity x . l)
  (define (s-p lst x)
    (if (null? lst)
        (list)
        (if (= (remainder (- (car l) x) 2) 0)
            (cons (car l) (s-p (cdr lst x)))
            (s-p (cdr lst) x))))
  (s-p l x))
;(same-parity 1 2 3 4 5 6 7)

;EX 2.21
(define (square-list-ex2-21-1 items)
  (if (null? items)
      (list)
      (cons (* (car items) (car items)) (square-list-ex2-21-1 (cdr items)))))
(define (square-list-ex2-21-2 items)
  (map (lambda (x) (* x x)) items))
;(square-list-ex2-21-1 (list 1 2 3 4 5))
;(square-list-ex2-21-2 (list 1 2 3 4 5))

;EX 2.23
(define (my-for-each factor items)
  (define (work items)
    (if (null? items)
        #t
        (begin
          (factor (car items))
          (work (cdr items)))))
  (work items))
;(my-for-each (lambda (x) (displayln x)) (list 1 3 2 4))

;EX 2.25
;(car (cdr (car (cddr (list 1 3 (list 5 7) 9)))))
;(car (car (list (list 7))))

;EX 2.27
(define (reverse-tree lst)
  (cond ((null? lst) (list))
        ((list? (car lst))
         (append (reverse-tree (cdr lst)) (list (reverse-tree (car lst)))))
        (else
         (append (reverse-tree (cdr lst)) (list (car lst))))))

;EX 2.28
(define (resolve l)
  (cond ((null? l) (list))
        ((list? (car l))
         (append (resolve (car l)) (resolve (cdr l))))
        (else
         (append (list (car l)) (resolve (cdr l))))))

;EX 2.30
(define (square-tree tree)
  (cond ((null? tree) tree)
        ((list? (car tree))
         (cons (square-tree (car tree)) (square-tree (cdr tree))))
        (else
         (cons (* (car tree) (car tree)) (square-tree (cdr tree))))))
;(square-tree (list 1 (list 2 (list 3 4) 5)))

;EX 2.31
(define (tree-map factor tree)
  (cond ((null? tree) tree)
        ((list? (car tree))
         (cons (tree-map (car tree)) (tree-map (cdr tree))))
        (else
         (cons (factor (car tree)) (tree-map (cdr tree))))))

;EG 2.2.3.1
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;EX 2.33
(define (map-ex2-33 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))
(define (append-ex2-33 seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length-ex2-33 sequence)
  (accumulate (lambda (x y)
                (+ 1 y)) 0 sequence))

;EX 2.34 - Horner
(define (hornor-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
;(hornor-eval 2 (list 1 3 2))

;EX 2.35 - count-leaves
(define (count-leaves-ex2-35 t)
  (foldr + 0 (map (lambda (t)
                    (if (list? t)
                        (count-leaves-ex2-35 t)
                        1))
                  t)))
;(count-leaves-ex2-35 (list 1 3 (list 5 9 (list 2 6) 5 7)))

;EX 2.36 - 对应元素求和
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map (lambda (t) (car t)) seqs))
            (accumulate-n op init (map (lambda (t)
                                         (if (list? t)
                                             (cdr t)
                                             (list))) seqs)))))
;(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;EX 2.37 - 矩阵计算
;(define matrix-ex2-37 (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (t) (dot-product t v)) m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (t)
           (map (lambda (c)
                  (dot-product t c))
                cols))
         m)))

;EX 2.38 - fold-left
(define fold-right foldr)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
;(foldr / 1 (list 3 2 5 6))
;(fold-left / 1 (list 3 2 5 6))
;(foldr list '() (list 1 2 3))
;(fold-left list '() (list 1 2 3))
        
;EX 2.39 - foldr/folds完成reverse
(define (reverse-fold-right sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reverse-fold-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
;(reverse-fold-right (list 1 4 2 5))
;(reverse-fold-left (list 1 4 2 5))

;EG 2.2.3.2 - 和为素数的数对（嵌套映射）
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((= (remainder n test-divisor) 0) test-divisor)
          (else (find-divisor n (+ 1 test-divisor)))))
  (= n (smallest-divisor n)))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
;(prime-sum-pairs 5)

;EG 2.2.3.3 - 全排列
(define (deep-remove item seq)
  (filter (lambda (x) (not (= x item))) seq))
(define (permutations-eg2-2-3-3 s)
  (if (null? s)
      '('())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
;(permutations-eg2-2-3-3 '(1 2 3))

;EX 2.40 - unique-pairs简化
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs-ex2-40 n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;EX 2.41 - 和为s的有序序列
(define (sum-s? s)
  (lambda (l) (= s (+ (car l) (cadr l) (caddr l)))))
(define (get-cols s)
  (let ((l (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list k j i))
                            4         (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 2 (- i 1))))
                    (enumerate-interval 3 (- s 3)))))
    (filter (sum-s? s) l)))
;(get-cols 10) 
                     
;EX 2.42 - 八皇后
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
(define (safe? k position)
    (iter-check (car position) (cdr position) 1))
(define (iter-check row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens) 
        #t
        (let ((row-of-current-queen (car rest-of-queens)))
            (if (or (= row-of-new-queen row-of-current-queen)          
                    (= row-of-new-queen (+ i row-of-current-queen))    
                    (= row-of-new-queen (- row-of-current-queen i)))   
                #f
                (iter-check row-of-new-queen 
                            (cdr rest-of-queens)  
                            (+ i 1))))))          
(define (queens board-size)
  (define (queen-cols k)
    (if (= 0 k)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (begin
    (map (lambda (lst) (reverse lst)) (queen-cols board-size))))
;(queens 8)
          
;EX 2.54 - symbol-equal?
(define (equal-ex2-54? lst1 lst2)
  (cond ((and (null? lst1) (null? lst2)) #t)
        ((or (null? lst1) (null? lst2)) #f)
        ((xor (list? (car lst1)) (list? (car lst2))) #f)
        ((list? (car lst1)) (and (equal-ex2-54? (car lst1) (car lst2))
                           (equal-ex2-54? (cdr lst1) (cdr lst2))))
        (else (and (eq? (car lst1) (car lst2))
                   (equal-ex2-54? (cdr lst1) (cdr lst2))))))
;(equal-ex2-54? '((this) (is a) list) '(this (is a) list))
             
;EG 2.3.2.1 - 符号求导
(define (=number? x num)
  (and (number? x) (= x num)))
(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))
(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        (else (list '+ x y))))
(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else (list '* x y))))
(define (sum? x) (and (list? x) (eq? (car x) '+)))
(define (product? x) (and (list? x) (eq? (car x) '*)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (displayln "error: unknown expression type -- DERIV"))))
(deriv '(* (* x y) (+ x 3)) 'x)

;EX 2.57 & EX 2.58   showed in SICP/homework/deriv.rkt


