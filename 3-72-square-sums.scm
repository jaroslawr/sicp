(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

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

(define (stream-match stream match-fn how-many)
  (define (match stream how-many-left result)
    (if (eq? how-many-left 0)
        result
        (if (or (eq? result '())
                (eq? (match-fn (stream-car stream)) (match-fn (car result))))
            (match
             (stream-cdr stream)
             (- how-many-left 1)
             (cons (stream-car stream) result))
            #f)))
  (let ((matching (match stream how-many '())))
    (if matching
        (cons-stream
         (append (list (match-fn (stream-car stream))) matching)
         (stream-match (stream-cdr stream) match-fn how-many))
        (stream-match (stream-cdr stream) match-fn how-many))))
    
(define square-sums
  (let ((sum-of-squares
         (lambda (x) (+ (* (car x) (car x))
                        (* (cadr x) (cadr x))))))
    (stream-match
     (weighted-pairs integers integers sum-of-squares)
     sum-of-squares
     3)))
