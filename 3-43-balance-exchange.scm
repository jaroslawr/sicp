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

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;;; Let's say we have three accounts A1, A2, A3, respectively
;;; containing 10, 20 and 30 dollars. Executing this procedure, we
;;; always deal with an account Ax with a given balance, another
;;; account Ay with a balance and we transfer the difference between
;;; the balances from one account to another. So, we can enumerate the
;;; sets of possible balances of the accounts supplied to exchange,
;;; together with the amount that will get withdrawn from Ax and
;;; deposited into Ay.
;;;
;;; {{10, 20}, -10} -> {10 - -10, 20 + -10} -> {20, 10}
;;; {{20, 10},  10} -> {20 -  10, 10 +  10} -> {10, 20}
;;; {{10, 30}, -20} -> {10 - -20, 30 + -20} -> {30, 10}
;;; {{30, 10},  20} -> {30 + -20, 10 - -20} -> {10, 30}
;;; {{20, 30}, -10} -> {20 - -10, 30 + -10} -> {30, 20}
;;; {{30, 20},  10} -> {30 + -10, 20 - -10} -> {20, 30}
;;;
;;; As one can see, the set of balances in this triple is closed under
;;; the exchange operation - the hypothetical concurrency mentioned
;;; doesn't matter as long as the exchange calls are sequential.
;;;
;;; If we, however, allow concurrent execution of withdraw and
;;; deposit, it is easy to provide a counterexample where this is not
;;; the case:
;;;
;;; A1: 10                    A2: 20                    A3: 30
;;;
;;; (exchange A1 A2)
;;;                          (exchange A2 A3)
;;;                          (balance A2)
;;;                          (balance A3)
;;; (balance A1)
;;; (balance A2)
;;; (withdraw -10)
;;;
;;; A1: 20                   A2: 20                     A3: 30
;;;
;;;                          (withdraw -10)
;;;
;;; A1: 20                   A2: 30                     A3: 30
;;;
;;;                          (deposit -10)
;;;
;;; A1: 20                   A2: 20                     A3: 30
;;;
;;;                                                     (deposit -10)
;;;
;;; A1: 20                   A2: 20                     A3: 20
;;;
;;; The total amount of money in this version is still preserved,
;;; because the calculated difference is always withdrawn from one
;;; account, and deposited to another one, so no money is added or
;;; removed from the system as a whole. The problem is that the
;;; difference might be calculated at the wrong point in time - while
;;; there is still an operation running on the other account, but this
;;; doesn't change the total sum of arguments to withdraw and deposit.
;;;
;;; If the operations inside a single account are not serialized, then
;;; one of the current withdrawals or deposits can overwrite the
;;; result of another, so even this condition will not be met then.
