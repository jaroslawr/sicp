(define (make-node data prev next)
  (cons data (cons prev next)))

(define (node-data node)
  (car node))

(define (node-prev node)
  (cadr node))

(define (node-next node)
  (cddr node))

(define (set-node-prev! node prev)
  (set-car! (cdr node) prev))

(define (set-node-next! node next)
  (set-cdr! (cdr node) next))

(define (make-deque)
  (cons '() '()))

(define (head-node deque)
  (car deque))

(define (tail-node deque)
  (cdr deque))

(define (set-head-node! deque head)
  (set-car! deque head))

(define (set-tail-node! deque tail)
  (set-cdr! deque tail))

(define (empty-deque? deque)
  (or (null? (head-node deque)) (null? (tail-node deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "Called FRONT for an empty deque")
      (node-data (head-node deque))))

(define (rear-deque deque)
    (if (empty-deque? deque)
      (error "Called REAR for an empty deque")
      (node-data (tail-node deque))))

(define (front-insert-deque! deque data)
  (let ((node (make-node data '() (head-node deque))))
    (cond ((empty-deque? deque)
           (set-head-node! deque node)
           (set-tail-node! deque node))
          (else
           (set-head-node! deque node)
           (set-node-prev! (head-node deque) node)))
    'success))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "Called DELETE for an empty deque")
      (let ((deleted (node-data (head-node deque))))
        (set-head-node! deque (node-next (head-node deque)))
        (cond ((not (null? (head-node deque)))
               (set-node-prev! (head-node deque) '())
               deleted)
              (else deleted)))))

(define (rear-insert-deque! deque data)
  (let ((node (make-node data (tail-node deque) '())))
    (cond ((empty-deque? deque)
           (set-tail-node! deque node)
           (set-head-node! deque node))
          (else
           (set-tail-node! deque node)
           (set-node-next! (tail-node deque) node)))
    'success))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "Called DELETE for an empty deque")
      (let ((deleted (node-data (tail-node deque))))
        (set-tail-node! deque (node-prev (tail-node deque)))
        (cond ((not (null? (tail-node deque)))
               (set-node-next! (tail-node deque) '())
               deleted)
              (else deleted)))))
