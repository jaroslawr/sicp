;;; Sometimes the operator can be a delayed object itself, consider
;;; the following example:

(define (identity proc)
  proc)

((identity (lambda (x) (* x x))) 2)

;;; An actually useful use case for this would be to have a procedure
;;; that wraps another procedure with some extra functionality under
;;; given conditions, otherwise simply returning the original one.
