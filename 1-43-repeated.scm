(define (square  x) (* x x))

(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter i result)
    (if (= i n)
        result
        (repeated-iter (+ i 1) (compose f result))))
  (repeated-iter 1 f))

((repeated square 2) 5)
