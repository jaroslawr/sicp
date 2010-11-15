(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))


;;; The possible outcomes are now:
;;;
;;; 101 - First all of P1 executes, then all of P2 executes
;;; 121 - First all of P2 executes, then all of P1 executes
;;; 100 - P1 computes the square of x to be 100, , P2 sets x to 11, P1 assigns 100 to x
