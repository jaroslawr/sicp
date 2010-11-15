(define (sign-change-detector a b)
  (cond ((and (> a 0) (< b 0)) -1)
        ((and (< a 0) (> b 0)) 1)
        (else 0)))

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

;; (define (make-zero-crossing input-stream last-value)
;;   (cons-stream
;;    (sign-change-detector (stream-car input-stream) last-value)
;;    (make-zero-crossing (stream-cdr input-stream)
;;                        (stream-car input-stream))))

(define zero-crossings
  (stream-map sign-change-detector (cons-stream 0 sense-data) sense-data))
