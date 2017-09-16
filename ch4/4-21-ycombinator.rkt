(define factorial
  (lambda (n)
    ((lambda (fact)
       (fact fact n))
     (lambda (ft k)
       (if (= k 1)
           1
           (* k (ft ft (- k 1))))))))

(define fibonacci
  (lambda (n)
    ((lambda (real-fib)
       (real-fib real-fib n))
     (lambda (fib k)
       (if (or (= k 0) (= k 1))
           1
           (+ (fib fib (- k 1)) (fib fib (- k 2))))))))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
