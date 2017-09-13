(define (reverse list)
  (define (reverse-iter rest result)
    (if (null? rest)
        result
        (reverse-iter (cdr rest) (cons (car rest) result))))
  (reverse-iter list '()))
