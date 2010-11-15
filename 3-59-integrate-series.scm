(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define integer-converses (stream-map (lambda (x) (/ 1 x)) integers))

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
