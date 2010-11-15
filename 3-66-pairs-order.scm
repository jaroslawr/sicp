(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define integer-pairs (pairs integers integers))

;; Before the pair (1,100) there is 99 + 98 = 197 other pairs. Because the cdr of
;; the initial (pair integers integers) stream is an interleave of
;; pairs of the form (1 y) and the rest of the pairs, every second
;; pair in the integer-pairs stream is of the form (1 y), where y are
;; consecutive integers. Here is a general formula:

(define (index-of-pair x y)
  (define (sum a b expr)
    (define (sum-iter a b expr result)
      (if (< a b)
          (sum-iter (+ a 1) b expr (+ result (expr a)))
          result))
    (sum-iter a b expr 0))
  (let ((offset (- y x)))
    (+
     (sum 1 x (lambda (x) (expt 2 x)))
     (* (expt 2 x) offset)
     (- (expt 2 (- x 1))))))
