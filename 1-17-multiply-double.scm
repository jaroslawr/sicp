(define (my-double x)
  (* 2 x))

(define (my-halve x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-mul a b)
  (cond
   ((= b 1) a)
   ((even? b) (fast-mul (my-double a) (my-halve b)))
   (else (+ a (fast-mul a (- b 1))))))
