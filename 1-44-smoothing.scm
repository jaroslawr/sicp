(define dx 0.1)

(define (square  x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter i result)
    (if (= i n)
        result
        (repeated-iter (+ i 1) (compose f result))))
  (repeated-iter 1 f))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(((repeated smooth 10) square) 5)
