(define (square x)
  (* x x))

(define (power x n)
  (cond ((= n 0) 1)
        ((even? n)
         (square (power x (/ n 2))))
        (else
         (* x (power x (- n 1))))))

(define (carmichael? n)
  (define (carmichael-test a n)
    (if (= a n)
        #t
        (and (= (remainder (- (power a n) a) n) 0)
             (carmichael-test (+ a 1) n))))
  (carmichael-test 2 n))
