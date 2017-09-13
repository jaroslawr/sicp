(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(expand 1 7 10)
;;; 1 (expand 3/7 10)
;;; 1 4 (expand 2/7 10)
;;; 1 4 2 (expand 6/7 10)
;;; 1 4 2 ... (expand x/7 10)

(expand 3 8 10)
;;; 3 (expand 6/8 10)
;;; 3 7 (expand 4/8 10)
;;; 3 7 5 (expand 0/8 10)
;;; 3 7 5 0 (expand 0/8 10)
;;; 3 7 5 0 0 (expand 0/8 10)
;;; 3 7 5 0 0 0 ... (expand 0/8 10)

;;; This stream is the stream of consecutive digits of a rational
;;; number in a system of base radix. Specifically, if the radix is
;;; ten, it yields the digits of the decimal expansion of a rational
;;; number.
