(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit stream tolerance)
  (define (stream-limit-helper stream tolerance previous)
    (let ((last (stream-car stream)))
      (if (< (abs (- last previous)) tolerance)
          last
          (stream-limit-helper (stream-cdr stream) tolerance last))))
  (stream-limit-helper (stream-cdr stream) tolerance (stream-car stream)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
