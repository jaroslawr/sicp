(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;;; try again
            ((eq? m 'release)
             (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))
  
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;;; a)

(define (make-nary-mutex n)
  (let ((inner-mutex (make-mutex))
        (height 0))
    (define (sync p)
      (begin
        (inner-mutex 'acquire)
        (p)
        (inner-mutex 'release)))
    (define (the-mutex m)
      (cond
       ((eq? m 'acquire)
        (sync
         (lambda ()
           (if (< height n)
               (set! height (+ height 1))
               (the-mutex 'acquire)))))
       ((eq? m 'release)
        (sync
         (lambda ()
           (if (> height 0)
               (set! height (- height 1))))))))
    the-mutex))

;;; b)

(define (make-nary-mutex-with-test n)
  (let ((cell (list false))
        (height 0))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)
                 (if (< height n)
                     (begin
                       (set! height (+ height 1))
                       (clear! cell))
                     (begin
                       (clear! cell)
                       (the-mutex 'acquire)))))
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-mutex 'release)
                 (begin
                   (if (> height 0)
                       (set! height (- height 1)))
                   (clear! cell))))))
    the-mutex))
