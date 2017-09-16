(define (square x) (* x x))

(define (make-from-real-imag x y)
  (lambda (op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) x)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle)
           (atan y x))
          (else
           (error "Unknown message")))))

(define (make-from-mag-ang mag ang)
  (lambda (op)
    (cond ((eq? op 'real-part) (* (cos ang) mag))
          ((eq? op 'imag-part) (* (sin ang) mag))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
           (error "Unknown message")))))
