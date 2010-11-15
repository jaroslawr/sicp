(define (for-each fn items)
  (unless (null? items)
    (fn (car items))
    (for-each fn (cdr items))))
