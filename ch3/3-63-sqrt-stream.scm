(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses))))

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

;;; By not giving an identity to the stream, this procedure "works
;;; around" the memoization, using sqrt-improve n! times to compute
;;; the n-th approximation, where the upper version only has to use it
;;; n times.
