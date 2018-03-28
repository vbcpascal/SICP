#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (inc n)
  (+ n 1))

;EG 1.3.1.1 - 求PI
(define (pi-sum sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (if (> a b)
      sum
      (pi-sum (+ sum (pi-term a)) (pi-next a) b)))
;(let ((b (read))) (displayln (* 8 (pi-sum 0 1 b))))

;EG 1.3.1.2 - 累积求积分
(define (integral f a b dx sum)
  (define (integral-next x)
    (+ x dx))
  (if (> a b)
      (* sum dx)
      (integral f (integral-next a) b dx (+ sum (f (+ a (/ dx 2)))))))
(define (fx-eg1-3-2 x) (* x x x))
;(integral fx-eg1-3-2 0 1 0.01 0)  ;change f,a,b,dx to calculate integral

;EX 1.29 - 辛普森规则求积分
(define (simpson f a b n k sum)
  (let ((coefficient (cond ((= k 0) 1)
                           ((= k n) 1)
                           ((= (remainder k 2) 0) 2)
                           (else 4))))
    (if (> k n)
        (* sum (/ (- b a) n 3))
        (simpson f a b n (+ k 1) (+ sum (* coefficient (f (+ a (* k (/ (- b a) n))))))))))
(define (fx-ex1-29 x) (* x x x))
;(simpson fx-ex1-29 0 1 1000 0 0)  ;change f,a,b,n to calculate integral

;EX 1.31 - John Wallis 求PI
(define (product-1-31 term a next b)
  (if (> a b)
      1
      (* (term a) (product-1-31 term (next a) next b))))
(define (fx-ex1-31 a) (* (/ (- a 1.0) a) (/ (+ a 1.0) a)))
(define (fn-ex1-31 a) (+ a 2))
;(* (product-1-31 fx-ex1-31 3 fn-ex1-31 1000000) 4)

;EX 1.32 - accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (sum-ex1-32 term a next b)
  (accumulate (lambda (x y) (+ x y)) 0 term a next b))
(define (product-ex1-32 term a next b)
  (accumulate (lambda (x y) (* x y)) 1 term a next b))

;EG 1.3.3.1 - 二分法求根
(define (search-root f neg-point pos-point)
  (define (close-enough?)
    (< (- pos-point neg-point) 0.00001))
  (define (positive? x)
    (< (* x (f neg-point)) 0))
  (define (negative? x)
    (< (* x (f pos-point)) 0))
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough?)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value) (search-root f neg-point mid-point))
                ((negative? test-value) (search-root f mid-point pos-point))
                (else mid-point))))))
(define (fx-eg1-3-3-1 x) (sin x))
;(search-root fx-eg1-3-3-1 2.0 4.0)

;EG 1.3.3.2 - 求不动点
(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (try guess-val)
    (let ((next-val (f guess-val)))
      (begin
        (display guess-val)  ;for ex 1.36
        (display ",")        ;for ex 1.36
        (if (close-enough? next-val guess-val)
            next-val
            (try next-val)))))
  (try first-guess))
(define (fx-eg1-3-3-2 x) (cos x))
;(fixed-point fx-eg1-3-3-2 1.0)

;EG 1.3.3.3 - 求不动点（平均值）
(define (fixed-point-average f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.000000000001))
  (define (try guess-val)
    (let ((next-val (average guess-val (f guess-val))))
      (begin
        (display guess-val)  ;for ex 1.36
        (display ",")        ;for ex 1.36
        (if (close-enough? next-val guess-val)
            next-val
            (try next-val)))))
  (try first-guess))
(define (fx-eg1-3-3-3 x) (cos x))
;(fixed-point-average fx-eg1-3-3-3 1.0)

;EX 1.35 - 不动点求黄金分割率
;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)  ;include "EG 1.3.3.2"

;EX 1.36 - 不动点求x^x=1000的根
;(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)  ;include "EG 1.3.3.2"

;EX 1.37 - 无穷连分式求黄金分割率
(define (cont-frac-iter N D k)
  (define (iter curr result)
    (if (= curr 0)
        result
        (iter (- curr 1) (/ (N curr) (+ (D curr) result)))))
  (iter k 0))
;(cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) 1000)

;EX 1.38 - 莱昂哈德·欧拉连分式求e
(define (D-ex1-38 x)
  (let ((rem (remainder x 3)))
    (if (= rem 2)
        (* 2 (+ 1 (quotient x 3)))
        1)))
;(+ 2 (cont-frac-iter (lambda (x) 1.0) D-ex1-38 1000))  ;include "EX 1.37"

;EX 1.39 - 连分式求tan
(define (tan-cf x k)
  (define (iter curr result)
    (if (= curr 1)
        (/ x (- 1 result))
        (iter (- curr 1) (/ (square x) (- (- (* 2 curr) 1) result)))))
  (iter k 0))
;(tan-cf (/ pi 3) 10000)

;EG 1.3.4.1 - 牛顿法
(define (newton-transform g)
  (define (deriv g)
    (define dx 0.00001)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))
(define (g-eg1-3-4-1 y) (- (square y) 81))
;(newton-method g-eg1-3-4-1 8.8)  ;include "EG 1.3.3.2"

;EG 1.3.4.2 - 抽象的不能再抽象的脑袋疼的抽象过程
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (sqrt-fixed x)
  (fixed-point-of-transform (lambda (y) (/ x y)) (lambda (f) (lambda (x) (average x (f x)))) 1.0))
(define (sqrt-newton x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))
;(sqrt-fixed 12) (sqrt-newton 12)

;EX 1.40 - 牛顿法求三次方程的根
(define (g-ex1-40 a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
;(newton-method (g-ex1-40 1 2 3) 1)

;EX 1.41 - double过程
(define (double f)
  (lambda (x) (f (f x))))
;(((double (double double)) inc) 5)

;EX 1.42 - 复合过程
(define (compose f g)
  (lambda (x) (f (g x))))
;((compose square inc) 6)

;EX 1.43 - 重复过程
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))  ;include "EX 1.42"
;((repeated square 2) 5)

;EX 1.44 - 平滑函数
(define (smooth f)
  (lambda (x)
    (define dx 0.00001)
    (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3)))
(define (fx-ex1-44 x) (sin x))
;(((repeated smooth 10) fx-ex1-44) (/ pi 2))  ;include "EX 1.44"

;EX 1.45 - n次平均阻尼
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (sqrt-n x n times)
  (define (fx-ex1-45 y) (/ x (expt y (- n 1))))
  (fixed-point-of-transform fx-ex1-45 (repeated average-damp times) 1.0))
(define (reliable-nth-root x n)
  (define times (floor (log n 2)))
  (sqrt-n x n times))
;(reliabl 2 4)

;EX 1.46 - 迭代式改进
(define (iterative-improve good-enough improve)
  (lambda (x)
    (if (good-enough x)
        x
        ((iterative-improve good-enough improve) (improve x)))))

