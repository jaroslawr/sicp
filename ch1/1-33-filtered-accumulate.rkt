(define (inc a) (+ a 1))
(define (ident a) a)
(define (sum a b) (+ a b))
(define (product a b) (* a b))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x) (* x x))

(define (filtered-accumulate first last term next null-value combiner filter)
  (define (iter current result)
    (if (> current last )
        result
        (if (filter current)
            (iter (next current) (combiner result (term current)))
            (iter (next current) result))))
  (iter first null-value))

(filtered-accumulate 1 10 square inc 0 sum prime?)

(define (product-of-relative-primes n)
  (filtered-accumulate 1 n ident inc 1 product (lambda (i) (= (gcd i n) 1))))
