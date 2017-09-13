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

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (w1 (weight s1car))
                (w2 (weight s2car)))
           (cond ((<= w1 w2)
                  (cons-stream s1car
                               (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (cons-stream s2car
                               (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t w)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) w)
    w)))

(define point-a (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))

(define point-b
  (let* ((filter (lambda (x) (not (or (eq? (remainder x 2) 0)
                                      (eq? (remainder x 3) 0)
                                      (eq? (remainder x 5) 0)))))
         (non-divisible-integers (stream-filter filter integers)))
    (weighted-pairs non-divisible-integers
                    non-divisible-integers
                    (lambda (x) (+ (* 2 (car x))
                                   (* 3 (cadr x))
                                   (* 5 (car x ) (cadr x)))))))
