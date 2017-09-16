(define next-account-serial-number
  (let ((next-number 0))
    (lambda ()
      (set! next-number (+ next-number 1))
      next-number)))

(define (make-numbered-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "No money on your account"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer))
        (serial-number (next-account-serial-number)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) protected)
            ((eq? m 'serial-number) serial-number)
            (else
             (error "Unknown operation -- MAKE-ACCOUNT" m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'serial-number) (account2 'serial-number))
        ((serializer1 (serializer2 exchange)) account1 account2)
        ((serializer2 (serializer1 exchange)) account1 account2))))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

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

;;; By chaining the serializers calls like in serialized-exchange, we
;;; want to extend the idea of a serializer from single objects to
;;; sets of objects. More precisely, we want to guarantee that
;;; whenever some procedure A starts operating on a set of objects O,
;;; then any other procedure B operating on some objects from O will
;;; have to wait with executing until A finishes. However, with an
;;; arbitrary ordering of seralizer calls for objects in O, we do not
;;; get this guarantee: A and B can start concurrently acquiring the
;;; serializers, until they both hit the serializer for the same
;;; object or until a deadlock arises. The situation is different if
;;; we always use a consistent ordering for O - then once A starts
;;; executing, it acquires the serializer for the first object in O,
;;; which becomes a "guard" element. B, or any other procedures using
;;; a subset of O, will first try to acquire the serializer of the
;;; same "guard" object, so they have to wait until A finishes to
;;; start doing anything with any other objects from O.
