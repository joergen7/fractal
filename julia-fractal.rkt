#lang racket
(require racket/draw)
(require racket/date)

(define ORIGIN-REAL 0)
(define ORIGIN-IMAG 0)
(define ZOOM 0.3)

(define SIZE 640)
(define MAX-MAGNITUDE 5)
(define MAX-EPOCH 100)
(define COLOR-FILLEDIN (make-object color% "white"))

(define C1 0.1+0.1i)
(define C2 0.2+0.2i)
(define C3 -0.6-0.3i)
(define C4 -1)
(define C5 -0.1+0.75i)
(define C6 0.25+0.52i)
(define C7 -0.5+0.55i)
(define C8 +0.66i)
(define C9 -i)
(define C10 -0.6772+0.3245i)

(define (rand)
  (* 2 (- (random) 0.5)))

; (define D0 0.5266313901029829+0.7859530042573404i)
; (define D1 0.4881178530697976+0.49753141856904515i)
; (define D2 -0.38358671818534773-0.43986914388201703i)



; Returns false if the complex number c is in the filled-in Julia set of the unary function f.
; Otherwise returns the epoch in which divergence was detected.
(define (diverges? f c epoch)
  (if (> epoch MAX-EPOCH)
      #f
      (if (> (magnitude c) MAX-MAGNITUDE)
          epoch
          (diverges? f (exact->inexact (f c)) (+ epoch 1)))))

(define get-color
  (let ([color-vec (make-vector 256 #f)])
    (λ (x)
      (define cached (vector-ref color-vec x))
      (if cached
          cached
          (let ([color (make-object color% x
                                           (min 255 (quotient (* x x) 10))
                                           (min 255 (exact-floor (* (sqrt x) 10))))])
            (vector-set! color-vec x color)
            color)))))

; Returns a color object for a given function f and complex number c
(define (color-at f c)
  (define d (diverges? f c 0))
  (if d
      (get-color (quotient (* d 255) MAX-EPOCH))
      COLOR-FILLEDIN))

(define (get-complex x y size)
  (define r (/ (- x (/ size 2)) size ZOOM))
  (define i (/ (- (/ size 2) y) size ZOOM))
  (make-rectangular (- r ORIGIN-REAL) (- i ORIGIN-IMAG)))
  
(define (polynomial-kernel z coeff-lst acc)
  (+ (* (first coeff-lst) z z)
     (* (second coeff-lst) z)
     (third coeff-lst)))


      

(define (render-polynomial polynomial size)

  (define (kernel z)
    (polynomial-kernel z polynomial 0))
  
  (define bitmap (make-object bitmap% size size))
  (define bitmap-dc (make-object bitmap-dc% bitmap))

  (time
   (for ([x (range size)])
     (for ([y (range size)])
       (define c (get-complex x y size))
       (define color (color-at kernel c))
       (send bitmap-dc set-pixel x y color))))

  bitmap)



#|
(for ([_ (range 128)])
  (define polynomial
    (list (make-rectangular (rand) (rand))
          (make-rectangular (rand) (rand))
          (make-rectangular (rand) (rand))))

  (define bitmap
    (render-polynomial polynomial SIZE))

  (define prefix
    "julia-square")

  (define timestamp
    (parameterize ([date-display-format 'iso-8601])
      (date->string (current-date))))

  (define output-file
    (string-append
     (string-join (append (list prefix timestamp)
                          (map number->string polynomial)) "_")
     ".png"))

  (send bitmap save-file output-file 'png))
|#
  (define polynomial
    (list (make-rectangular (rand) (rand))
          (make-rectangular (rand) (rand))
          (make-rectangular (rand) (rand))))

  (define bitmap
    (render-polynomial polynomial SIZE))

bitmap