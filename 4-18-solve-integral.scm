;;; (lambda <parameters>
;;;   (let ((u '*unassigned*)
;;;         (v '*unassigned*))
;;;     (let ((a <e1>)
;;;           (b <e2>))
;;;       (set! u a)
;;;       (set! v b))
;;;     <e3>))
;;;
;;; As the let binding variables a and b will get expanded to an
;;; application of lambda, both <e1> and <e2> will get evaluated while
;;; u and v are still both '*unassigned*. So with solve:
;;;
;;; (define (solve f y0 dt)
;;;   (define y (integral (delay dy) y0 dt))
;;;   (define dy (stream-map f y))
;;;   y)
;;;
;;; The second definition of will fail, because at the moment of
;;; evaluation of (stream-map f y), y will still be '*unassigned*.
