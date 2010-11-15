(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

(f (f (f (f x))))

(((double (double double)) inc) 5)

(((double (double double)) inc) 5)
