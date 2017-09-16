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

(define (stream-filter-cons stream filter)
  (let ((scar (stream-car stream))
        (scadr (stream-car (stream-cdr stream))))
    (if (filter scar scadr)
        (cons-stream scar
                     (stream-filter-cons (stream-cdr stream) filter))
        (stream-filter-cons (stream-cdr stream) filter))))

(define ramajuan
  (let ((weight
         (lambda (x) (+ (* (car x) (car x) (car x))
                        (* (cadr x) (cadr x) (cadr x))))))
    (stream-map
     (stream-filter-cons
      (weighted-pairs integers integers weight)
      (lambda (x y) (eq? (weight x) (weight y))))
     weight)))

;; 1 ]=> (stream-ref ramajuan 0)
;; ;Value: 1729

;; 1 ]=> (stream-ref ramajuan 1)
;; ;Value: 4104

;; 1 ]=> (stream-ref ramajuan 2)
;; ;Value: 13832

;; 1 ]=> (stream-ref ramajuan 3)
;; ;Value: 20683

;; 1 ]=> (stream-ref ramajuan 4)
;; ;Value: 32832

