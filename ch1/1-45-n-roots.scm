(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter i result)
    (if (= i n)
        result
        (repeated-iter (+ i 1) (compose f result))))
  (repeated-iter 1 f))

(define (root n x)
  (fixed-point
   ((repeated
     average-damp
     (truncate (/ (log n) (log 2))))
     (lambda (y) (/ x (expt y (- n 1)))))
    1.0))
