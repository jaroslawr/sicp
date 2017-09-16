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

(define (n k) 1.0)

(define (d k)
  (if (or (= k 1) (not (= (remainder (+ k 1) 3) 0)))
      1
      (* (quotient (+ k 1) 3) 2)))

(cont-frac-rec n d 11)
(cont-frac-iter n d 11)
