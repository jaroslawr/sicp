(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

;;; x -> (a b c d)
;;; y -> '()
;;;
;;; x -> (b c d)
;;; y -> (a)
;;;
;;; x -> (c d)
;;; y -> (b a)
;;;
;;; x -> (d)
;;; y -> (c b a)
;;;
;;; x -> ()
;;; y -> (d c b a)

;;; 'mystery' reverses the list supplied as argument "in place"
;;;
;;; v -------------------------+
;;;                            |
;;;       - -    - -    - -    |    - -
;;; w -> | | |->| | |->| | |-> +-> | | |
;;;       - -    - -    - -         - -
;;;       |      |      |           |
;;;      \|/    \|/    \|/         \|/
;;;       d      c      b           a
;;;

;;; v => (a)
;;; w => (d c b a)
