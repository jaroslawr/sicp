;;; Consider what happens with/without memoization when you do
;;; something like this:
;;;
;;; (map square (first-n-primes 1000))

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define (square x)
  (* x x))

;;; L-Eval:
(square (id 10))
;;; L-Eval value: 100 (both with memoization and without)

;;; L-Eval:
count
;;; L-Eval value: 1 with memoization, 2 with no memoization
