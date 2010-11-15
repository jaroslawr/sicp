(define (inc a) (+ a 1))

(define (ident a) a)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a) ))))
  (iter a null-value))

(define (product-iter term a next b)
  (accumulate-iter (lambda (a b) (* a b)) 1 term a next b))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

(define (product-rec term a next b)
  (accumulate-rec (lambda (a b) (* a b)) 1 term a next b))
