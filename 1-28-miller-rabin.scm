(define (square n)
  (* n n))

(define (expmod base exp m)
  (define (non-trivial-square-root x n)
    (and
     (not (or (= x (- n 1)) (= x 1)))
     (= (remainder (- (square x) 1) n) 0)))
  (cond ((= exp 0)
         1)
        ((even? exp)
         (if (non-trivial-square-root (expmod base (/ exp 2) m) m)
             0
             (remainder (square (expmod base (/ exp 2) m)) m)))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
