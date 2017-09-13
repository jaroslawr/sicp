(define (fringe tree)
  (define (fringe-iter tree result)
    (cond
     ((null? tree) result)
     ((list? (car tree))
      (fringe-iter (cdr tree) (append result (fringe (car tree)))))
     (else
      (fringe-iter (cdr tree) (append result (list (car tree)))))))
  (fringe-iter tree '()))

(fringe (list 5 6 (list 7 8) (list 9 (list 10 11))))
