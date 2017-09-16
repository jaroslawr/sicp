;;; This is an exercise in distinction between code and data. When you
;;; define map as a primitive, first the lambda will be evaluated by
;;; the child scheme interpreter, converted to its internal
;;; representation using make-procedure and then passed as data
;;; (without evaluation) to the parent interpreters map. 
;;;
;;; Think about the difference between those two:
;;; (map (quote (lambda (x) (display x))) (list 1 2 3))
;;; (map (lambda (x) (display x)) (list 1 2 3))
