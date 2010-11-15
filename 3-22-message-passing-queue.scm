(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (eq? front-ptr '()))
    (define (front-queue)
      (if (empty-queue?)
          '()
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-item (cons item '())))
        (cond
         ((empty-queue?)
          (set! front-ptr new-item)
          (set! rear-ptr new-item))
         (else
          (set-cdr! rear-ptr new-item)
          (set! rear-ptr new-item)))))
    (define (delete-queue!)
      (cond
       ((empty-queue?)
        (error "Called DELETE for an empty queue"))
       (else
        (set! front-ptr (cdr front-ptr)))))
    (define (dispatch m)
      (cond
       ((eq? m 'front) front-queue)
       ((eq? m 'insert) insert-queue!)
       ((eq? m 'delete) delete-queue!)
       (else
        (error "Unknown message - QUEUE"))))
    dispatch))
