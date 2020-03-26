#lang racket
(require plot)

(define X-MIN -2)
(define X-MAX 2)
(define Y-MIN -0.5)
(define Y-MAX 0.5)
(define SYM ".")
(define NEPOCH 20000)




(define (henon v)
  (define x0 (vector-ref v 0))
  (define y0 (vector-ref v 1))
  (define x1 (+ y0 1 (* -1.4 x0 x0)))
  (define y1 (* 0.3 x0))
  (vector-immutable x1 y1))

(define (expand f acc nepoch)
  (if (equal? nepoch 0)
      acc
      (let* ([v0 (first acc)]
             [v1 (f v0)])
        (expand f (cons v1 acc) (- nepoch 1)))))




(define seed (list (henon (henon (henon (vector-immutable 0 0))))))
(define fractal-set (expand henon seed NEPOCH))
  
  


(parameterize ([plot-x-label #f]
               [plot-y-label #f]
               [plot-width 500]
               [plot-height 500])
  (plot (points fractal-set
                #:x-min X-MIN #:x-max X-MAX
                #:y-min Y-MIN #:y-max Y-MAX
                #:sym SYM)))