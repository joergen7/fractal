#lang racket
(require rackunit)
(require 2htdp/image)

;;======================================
;; Data structures
;;======================================

(struct vec (x y))

;;======================================
;; Basic operations
;;======================================

(define (angle v)
  (define x (vec-x v))
  (define y (vec-y v))
  (cond
    [(equal? x 0) (if (< y 0) (* pi 3/2) (/ pi 2))]
    [else         (+ (if (< x 0) pi 0)
                     (atan (/ y x)))]))

(define (abs v)
  (define x (vec-x v))
  (define y (vec-y v))
  (sqrt (+ (* x x) (* y y))))


(define (scale v factor)
  (define x (* (vec-x v) factor))
  (define y (* (vec-y v) factor))
  (vec x y))

(define (rotate v delta-phi)
  (define r (abs v))
  (define phi (+ (angle v) delta-phi))
  (define x (* r (cos phi)))
  (define y (* r (sin phi)))
  (vec x y))

(define (add v1 v2)
  (define x1 (vec-x v1))
  (define y1 (vec-y v1))
  (define x2 (vec-x v2))
  (define y2 (vec-y v2))
  (define x (+ x1 x2))
  (define y (+ y1 y2))
  (vec x y))

;;======================================
;; Pattern operations
;;======================================

(define (scale-pattern pattern r)

  (define (scale-pair pair)
    (list (scale (first pair) r)
          (scale (second pair) r)))

  (map scale-pair pattern))

(define (rotate-pattern pattern phi)

  (define (rotate-pair pair)
    (list (rotate (first pair) phi)
          (rotate (second pair) phi)))

  (map rotate-pair pattern))

(define (transpose-pattern pattern v)

  (define (transpose-pair pair)
    (list (add (first pair) v)
          (second pair)))

  (map transpose-pair pattern))


;;======================================
;; Substitution
;;======================================

(define (subst pair pattern)
  (define vec1 (first pair))
  (define vec2 (second pair))
  (define phi (angle vec2))
  (define r (abs vec2))
  (define scaled-pattern (scale-pattern pattern r))
  (define rotated-pattern (rotate-pattern scaled-pattern phi))
  (transpose-pattern rotated-pattern vec1))

(define (step world pattern)
  
  (define (f pair)
    (subst pair pattern))

  (apply append (map f world)))

(define (run world pattern iterations)
  (if (equal? iterations 0)
      world
      (run (step world pattern) pattern (- iterations 1))))
      
  
;;======================================
;; Output
;;======================================

(define (draw-world world)

  (for/fold ([scene (empty-scene 800 300)])
            ([pair world])
    (define v1 (first pair))
    (define v2 (second pair))
    (define x1 (vec-x v1))
    (define y1 (vec-y v1))
    (define x2 (vec-x v2))
    (define y2 (vec-y v2))
    (scene+line scene x1 y1 (+ x1 x2) (+ y1 y2) 'black)))



;;======================================
;; Example
;;======================================

(define h (sqrt (- (* 1/3 1/3) (* 1/6 1/6))))
(define pattern1 (list (list (vec 0 0) (vec 1/3 0))
                       (list (vec 1/3 0) (vec 1/6 (- h)))
                       (list (vec 1/2 (- h)) (vec 1/6 h))
                       (list (vec 2/3 0) (vec 1/3 0))))

(define pattern1a (list (list (vec 0 0) (vec 0.3 -0.1))
                        (list (vec 0.3 -0.1) (vec 0.5 0.4))
                        (list (vec 0.8 0.3) (vec 0.05 -0.2))
                        (list (vec 0.85 0.1) (vec 0.15 -0.1))))

(define pattern2 (list (list (vec 0 0) (vec 1 0))
                       (list (vec 1 0) (vec 0.75 0.1))
                       (list (vec 1 0) (vec 1 -0.2))
                       (list (vec 0.8 0) (vec 2 0.2))
                       (list (vec 0.2 0) (vec 0.4 -0.2))))
(define seed1
  (let ((s 1500)
        (x0 100)
        (y0 450))

    (list (list (vec x0 y0)
                (vec (/ s 6) (* (- s) h)))
          (list (vec (+ x0 (/ s 6)) (+ y0 (* (- s) h)))
                (vec (/ s 6) (* s h)))
          (list (vec (+ x0 (/ s 3)) y0)
                (vec (/ s -3) 0)))))

(define seed2 (list (list (vec 40 290)
                          (vec 40 -20))))




(define image (draw-world (run seed2 pattern2 4)))
image

;;======================================
;; Unit tests
;;======================================

(check-equal? (angle (vec 1 0)) 0)
(check-equal? (angle (vec 1 1)) (/ pi 4))
(check-equal? (angle (vec 0 1)) (/ pi 2))
(check-equal? (angle (vec -1 1)) (* pi 3/4))
(check-equal? (angle (vec -1 0)) pi)
(check-equal? (angle (vec -1 -1)) (* pi 5/4))
(check-equal? (angle (vec 0 -1)) (* pi 3/2))
(check-equal? (angle (vec 1 -1)) (* pi -1/4))