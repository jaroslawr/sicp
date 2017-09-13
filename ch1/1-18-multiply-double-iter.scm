(define (my-double x)
  (* 2 x))

(define (my-halve x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-mul a b)
  (define (fast-mul-iter a b accumulator)
    (cond
     ((= b 1) (+ a accumulator))
     ((even? b) (fast-mul-iter (my-double a) (my-halve b) accumulator))
     (else (fast-mul-iter a (- b 1) (+ a accumulator)))))
  (fast-mul-iter a b 0))
