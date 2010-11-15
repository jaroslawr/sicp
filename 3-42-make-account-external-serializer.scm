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
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance)
               ((protected (lambda () balance))))
              (else (error "Unknown operation -- MAKE-ACCOUNT" m))))
      dispatch)))

;;; I don't really like that fact that you are supposed to solve this
;;; exercise before (make-serializer) being explained. I would say
;;; that in this way, the deposit will always wait for the withdraw to
;;; finish, never the other way around, but it all depends what
;;; happens inside of the (make-serializer) black box.
