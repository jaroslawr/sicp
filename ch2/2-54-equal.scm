(define (my-equal? a b)
  (cond
   ((null? a)
    (null? b))
   ((and (list? a) (list? b))
    (and
     (my-equal? (car a) (car b))
     (my-equal? (cdr a) (cdr b))))
   (else (eq? a b))))
