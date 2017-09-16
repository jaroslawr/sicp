(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i-stream v0)
    (add-streams
     (scale-stream i-stream R)
     (integral (scale-stream i-stream (/ 1 C))
               v0
               dt))))

(define RC1 (RC 5 1 0.5))

(define i-stream (stream 1 2 3 4 5))

(define v-stream (RC1 i-stream 10))
