(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

(factorial 5)

;;; With application-order evaluation this causes an infinite loop, as
;;; the arguments to unless are evaluated before the body of the
;;; unless procedure itself. It would work with normal-order
;;; evaluation.
