(define (sign-change-detector a b)
  (cond ((and (> a 0) (< b 0)) -1)
        ((and (< a 0) (> b 0)) 1)
        (else 0)))

(define sense-data
  (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

;;; a) Sense data values
;;; b) Averages
;;; c) Sign changes
;;;
;;; a) (stream        1     2      1.5      1      0.5     -0.1       -2      -3      -2       -0.5      0.2     3    4)
;;; b) (stream    0.5   1.5   1.75     1.25   0.75     0.3      -1.05    -2.5    -2.5    -1.25     -0.15     1.6   3.5)
;;; c) (stream        0     0       0       0       0       0         -1       0       0        0         0      1      0)

(define (make-zero-crossing input-stream last-value last-avg)
  (let ((avg (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector last-avg avg)
                 (make-zero-crossing (stream-cdr input-stream)
                                     (stream-car input-stream)
                                     avg))))

(define zero-crossings
  (make-zero-crossing sense-data 0 0))
