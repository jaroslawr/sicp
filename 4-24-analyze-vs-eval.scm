;;; I used the following benchmark:

(define before (get-universal-time))

(define (even? x)
  (= (remainder x 2) 0))

(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p)   (* q q))
                     (+ (* 2 p q) (* q q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

(define (iter i)
  (if (< i 1000)
    (begin (fib 1000) (iter (+ i 1))) ;;; Do not display the result
    '()))

(iter 0)
(display (- (get-universal-time) before))
(newline)

;;; The interpreter with analyze builtin took 20 seconds to run it,
;;; the traditional one - 33 seconds. But a lot depends on what the
;;; program does - the proportion between the relative amount of time
;;; the interpreter spends in control structures vs. in doing actual
;;; raw computations. For example just computing a fib 1000000 will
;;; take approximately the same amount of time with both interpreters.
