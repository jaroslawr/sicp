;;; (set! balance 100)
;;;
;;; Peter: (set! balance (+ balance 10))
;;; Paul:  (set! balance (- balance 20))
;;; Mary:  (set! balance (- balance (/ balance 2)))

;;; a) If the operations happen sequentianlly, there are 3! possible
;;; outcomes:
;;;
;;; If we first increase the balance by 10:
;;; 110->90->45
;;; 110->55->35
;;;
;;; If we first decrease the balance by 20:
;;; 80->90->45
;;; 80->40->30
;;;
;;; If we first decrease the balance by half:
;;; 50->60->40
;;; 50->30->40
;;; 

;;; b) If we allow the operations to interleave each other:
;;;
;;; Example 1:
;;;
;;; Balance    Peter                 Paul                   Mary                         
;;;
;;; 100
;;;            Read balance: 100
;;;                                  Read balance: 100
;;;            Deposit 10
;;;            New balance: 110
;;;                                  Withdraw 10
;;;                                  New balance: 90
;;;
;;; Example 2:
;;;
;;; Balance    Peter                 Paul                   Mary                         
;;;
;;; 100
;;;
;;;            Read balance: 100
;;;                                  Read balance: 100
;;;                                                         Read balance: 100
;;;                                  Withdraw 20
;;;                                  New balance: 80
;;;                                                         Withdraw 50
;;;                                                         New balance: 50
;;;            Withdraw 100
;;;            New balance: 0
