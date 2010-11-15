(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (RLC R L C dt)
  (lambda (il0 vc0)
    (define v-stream
      (integral (delay
                  (scale-stream i-stream (- (/ 1 C))))
                vc0
                dt))
    (define i-stream
      (integral (delay
                  (add-streams
                   (scale-stream v-stream (/ 1 L))
                   (scale-stream i-stream (- (/ R L)))))
                il0
                dt))
    (cons v-stream i-stream)))

(define RLC1 (RLC 1 1 0.2 0.1))

(define RLC1-streams (RLC1 0 10))

(define v-stream (car RLC1-streams))

(define i-stream (cdr RLC1-streams))
