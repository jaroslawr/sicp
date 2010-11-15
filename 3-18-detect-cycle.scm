(define (make-cycle x)
  (set-cdr! (last-pair x) (cdr x))
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (detect-cycle a-list)
  (define (detect-cycle-rec a-list visited)
    (cond ((null? a-list) #f)
          ((memq a-list visited) #t)
          (else
           (detect-cycle-rec (cdr a-list) (append visited (list a-list))))))
  (detect-cycle-rec a-list '()))
