(define ones
  (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define integer-converses
  (stream-map (lambda (x) (/ 1 x)) integers))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))
   
(define (integrate-series power-series)
  (mul-streams integer-converses power-series))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define add-series
  add-streams)

(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-series
    (scale-stream (stream-cdr s2) (stream-car s1))
    (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series stream)
  (define inversion
    (cons-stream 1
                 (mul-series           
                  (scale-stream (stream-cdr stream) -1)
                  inversion)))
  inversion)

(define (div-series s1 s2)
  (scale-stream
   (mul-series s1 (invert-unit-series s2))
   (/ 1 (stream-car s2))))

(define tan-series
  (div-series sine-series cosine-series))
