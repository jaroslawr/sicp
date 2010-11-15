;;; I don't think simultaneus definitions are truely possible,
;;; consider the following:
;;;
;;; ((lambda (x)
;;;    (define a (+ b x))
;;;    (define b (+ a x))
;;;    (+ a b)) 10)
;;;
;;; It is hard to say what would be reasonable behavior of such a
;;; program according to what Eva proposes. So I guess we have to live
;;; with what Liz proposes.
