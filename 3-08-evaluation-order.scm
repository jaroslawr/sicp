;;; (+ (f 0) (f 1))

(define state 0)

(define (f x)
  (let ((prev-state state))
    (set! state x)
    prev-state))
