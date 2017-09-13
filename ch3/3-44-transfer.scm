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
            (else
             (error "Unknown operation -- MAKE-ACCOUNT" m))))))

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;;; This procedure is OK as long it is used with a constant amount -
;;; the exchange procedure problem was that it relied heavily on the
;;; current state of the account, which could lead to inconsistencies
;;; if the state was changed by other running procedures. Here, as
;;; long as there is enough money on the from-account, the execution
;;; is independent from the accounts state.
