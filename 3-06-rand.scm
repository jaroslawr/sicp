(define (rand msg)
  (cond
   ((eq? msg 'generate) (random))
   ((eq? msg 'reset)
    (lambda (seed)
      (random-seed seed)))))
