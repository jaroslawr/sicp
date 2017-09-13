(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;;; The possible outcomes are:
;;;
;;; 100 * 100 * 100 - First all of P1 executes, then all of P2 executes
;;;
;;; 1000 * 1000 - First all of P2 executes, then all of P1 executes
;;;
;;; 10 * 1000 - First P1 reads x, then all of P2 executes, then P1
;;; does the second read of x
;;;
;;; 10 * 100 * 100 - First P2 reads, then all of P1 executes, then P2
;;; reads x two more times
;;;
;;; 10 * 10 * 100 - First P2 reads x two times, then all of P1
;;; executes, then P2 does the thrid read of x


(set! x 10)
(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

;;; With a serializer, the possible outcomes are:
;;;
;;; 100 * 100 * 100 - First all of P1 executes, then all of P2 executes
;;;
;;; 1000 * 1000 - First all of P2 executes, then all of P1 executes
