(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random 1.0) range))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (exact->inexact (/ passed (+ passed failed)))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (belongs-to-unit-circle? x y)
  (< (+ (* x x) (* y y)) 1))

(define (estimate-integral predicate x1 y1 x2 y2)
  (define (experiments)
    (cons-stream
     (let ((rand-x (random-in-range x1 x2))
           (rand-y (random-in-range y1 y2)))
       (predicate rand-x rand-y))
     (experiments)))
  (stream-map
   (lambda (number-of-positives) (* number-of-positives (* (- x2 x1) (- y2 y1))))
   (monte-carlo (experiments) 0 0)))

(define pi-stream (estimate-integral belongs-to-unit-circle? -1 -1 1 1))

(newline)
(display (stream-ref pi-stream 20000))
(newline)
