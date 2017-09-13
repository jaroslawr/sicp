(define (same-parity . items)
  (define (of-parity items parity result)
    (if (null? items)
        result
        (if (= (remainder (car items) 2) parity)
            (of-parity (cdr items) parity (append result (list (car items))))
            (of-parity (cdr items) parity result))))
  (of-parity (cdr items) (remainder (car items) 2) (list (car items))))
