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

(define (smooth input-stream)
  (cons-stream (/ (+ (stream-car input-stream) (stream-car (stream-cdr input-stream))) 2)
               (smooth (stream-cdr input-stream))))

(define (make-zero-crossing input-stream smooth-fn last-value)
  (define (make-zero-crossing-rec stream last-value)
    (cons-stream
     (sign-change-detector last-value (stream-car stream))
     (make-zero-crossing-rec (stream-cdr stream)
                             (stream-car stream))))
  (let ((smoothed (smooth-fn (cons-stream 0 input-stream))))
    (make-zero-crossing-rec smoothed 0)))

(define zero-crossings
  (make-zero-crossing sense-data smooth 0))
