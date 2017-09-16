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
            ((eq? m 'balance)
             ((protected (lambda () balance)))) ;;; Added serializer
            (else
             (error "Unknown operation -- MAKE-ACCOUNT" m))))))

;;; The hypothetical sense of the serializer could be to wait for all
;;; the withdraws and deposits to complete before we show the balance.
;;; Otherwise the balance returned may be inactual almost in the same
;;; moment it was returned. Other then, this doesn't seem to have any
;;; critical impact on the program.
