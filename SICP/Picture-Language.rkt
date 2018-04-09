#lang racket
;vect
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (scale-vect s v)
  (make-vect (* s (car v))
             (* s (cdr v))))
(define (add-vect v1 v2)
  (make-vect (+ (car v1) (car v2))
             (+ (cdr v1) (cdr v2))))
(define (neg-vect v)
  (scale-vect -1 v))
(define (sub-vect v1 v2)
  (add-vect v1 (neg-vect v2)))

;frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))
(define (frame-coord-map frame)
  (lambda (v) 
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "test frame" 850 440))
(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define (line start end) ((draw-line vp) start end "red"))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start-coord-map ((frame-coord-map frame) (start-segment segment)))
             (end-coord-map ((frame-coord-map frame) (end-segment segment))))
         (line
          (make-posn (xcor-vect start-coord-map) (ycor-vect start-coord-map))
          (make-posn (xcor-vect end-coord-map) (ycor-vect end-coord-map)))))
     segment-list)))

(define (points->segments points)
  (cond ((null? points) null)
        ((null? (cddr points))
         (list (make-segment (car points) (cadr points))))
        (else
         (cons (make-segment (car points) (cadr points))
               (points->segments (cdr points))))))

(define wave
  (let ((v1 (list (make-vect 0 26)
                  (make-vect 6 17)
                  (make-vect 12 25)
                  (make-vect 14 21)
                  (make-vect 10 0)))
        (v2 (list (make-vect 16 0)
                  (make-vect 21 13)
                  (make-vect 25 0)))
        (v3 (list (make-vect 31 0)
                  (make-vect 25 19)
                  (make-vect 41 6)))
        (v4 (list (make-vect 41 15)
                  (make-vect 31 27)
                  (make-vect 25 27)
                  (make-vect 27 35)
                  (make-vect 25 41)))
        (v5 (list (make-vect 16 41)
                  (make-vect 14 35)
                  (make-vect 16 27)
                  (make-vect 12 27)
                  (make-vect 6 25)
                  (make-vect 0 35)))
        (scale-to-1 (lambda (list-of-vect)
                      (map
                       (lambda (v) (scale-vect 0.024 v)) list-of-vect))))
    (segments->painter
     (foldr append null (map (compose1 points->segments
                                       scale-to-1)
                             (list v1 v2 v3 v4 v5))))))

(define (make-straightened-frame origin edge1 edge2)
  (make-frame (add-vect (make-vect 0 440)
                        (make-vect (xcor-vect origin)
                                   (- (ycor-vect origin))))
              (make-vect (xcor-vect edge1)
                         (- (ycor-vect edge1)))
              (make-vect (xcor-vect edge2)
                         (- (ycor-vect edge2)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ((m (frame-coord-map frame))
           (new-origin (m origin)))
      (painter
       (make-frame new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin))))))

(define (beside painter1 painter2)
  (let ((paint-left (transform-painter painter1
                                       (make-vect 0.0 0.0)
                                       (make-vect 0.5 0.0)
                                       (make-vect 0.0 1.0)))
        (paint-right (transform-painter painter2
                                       (make-vect 0.5 0.0)
                                       (make-vect 1.0 0.0)
                                       (make-vect 0.5 1.0))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

(define (below painter2 painter1)
  (let ((paint-up (transform-painter painter1
                                     (make-vect 0.0 0.5)
                                     (make-vect 1.0 0.5)
                                     (make-vect 0.0 1.0)))
        (paint-down (transform-painter painter2
                                       (make-vect 0.0 0.0)
                                       (make-vect 1.0 0.0)
                                       (make-vect 0.0 0.5))))
    (lambda (frame)
      (paint-up frame)
      (paint-down frame))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let* ((up (up-split painter (- n 1)))
             (right (right-split painter (- n 1)))
             (top-left (beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner)))))
(define (square-limit painter n)
  (let* ((quarter (corner-split painter n))
         (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))

(define frame-left
  (make-straightened-frame (make-vect 20 20)
                           (make-vect 400 0)
                           (make-vect 0 400)))
(define frame-right
  (make-straightened-frame (make-vect 550 100)
                           (make-vect 300 -50)
                           (make-vect -100 300)))

((square-limit wave 4) frame-left)
((beside wave
         (flip-vert wave))
 frame-right)
