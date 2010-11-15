(define (square x)
  (* x x))

(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (iterate guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
            next
            (iterate next))))
    (iterate first-guess)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve good-enough? f) first-guess))

(define (average x y) (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (define (good-enough? guess improved-guess)
    (< (abs (- (square improved-guess) x)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) x))
