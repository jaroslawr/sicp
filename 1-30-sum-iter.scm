(define (even? x)
  (= (remainder x 2) 0))

(define (inc x)
  (+ x 1))

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (define (integrate f a b n h)
    (define (expr k)
      (define (coeff k)
        (cond ((or (= k 0) (= k n)) 1)
              ((even? k) 4)
              (else 2)))
      (* (coeff k) (f (+ a (* k h)))))
    (* (/ h 3.0) (sum expr 0 inc n)))
  (integrate f a b n (/ (- b a)  n)))
