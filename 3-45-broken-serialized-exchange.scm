(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "No money on your account"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) protected)
            (else
             (error "Unknown operation -- MAKE-ACCOUNT" m))))))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;;; The problem here is that you can not use the same serializer for a
;;; nested call two times, for example:
;;;
;;; (define (square n) (* n n))
;;;
;;; (define s (make-serializer))
;;;
;;; ((s (s square)) 2)
;;;
;;; The lambda-expression created by (s (s square)) tries to acquire
;;; the lock two times before running the square procedure, which ends
;;; up in an infinite loop, as the lock has no chance to get released,
;;; as it only happens after the execution. The same thing happens to
;;; serialized-exchange in this exercise.
