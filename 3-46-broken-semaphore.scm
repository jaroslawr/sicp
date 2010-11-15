(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

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

;;; Assuming we have two processes, X and Y, this breaks in the
;;; following scenario:
;;;
;;; X creates a serializer
;;; Y creates a serializer
;;; X runs its "serialized" procedure
;;; Y runs its "serialized" procedure
;;; X calls test-and-set!
;;; X'es test-and-set! '(car cell)' returns false
;;; Y calls test-and-set!
;;; Y's test-and-set! '(car cell)' returns false
;;; X'es test-and-set! sets the car of cell to true
;;; Y's test-and-set! sets the car of cell to true
;;;
;;; In this scenario the procedures run concurrently just as if they
;;; were no serializer used.
