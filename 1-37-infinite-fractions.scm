(define (cont-frac-rec n d k)
  (define (cont-frac i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac (+ i 1))))))
  (cont-frac 1))

(define (cont-frac-iter n d k)
  (define (cont-frac i result)
    (if (= i 0)
        result
        (cont-frac (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac (- k 1) (/ (n k) (d k))))

(cont-frac-rec  (lambda (i) 1.0) (lambda (i) 1.0) 15)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 15)
