(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;;; L-Eval:
count
;;; L-Eval value:
;;; (The outer "id" is evaluated and returns the thunk of the delayed
;;; inner "id", which wasn't yet evaluated because it wasn't needed)
1

;;; L-Eval:
w
;;; L-Eval value:
;;; Now the inner "id" is evaluated.
10

;;; L-Eval:
count
;;; L-Eval value:
2
