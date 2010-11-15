;;; Unfinished and possibly totally incorrect?

;;; Consider the following example, implementing a general abstraction
;;; for the situation describe in the book as the hint for this
;;; exercise:

;;; Shared list of shared resources

(define resources-registry
  (let ((resources (list))
        (serializer (make-serializer)))
    (lambda (msg)
      (define (append resource)
        (set! resources (cons resource resources)))
      (define (fetch)
        resources)
      (cond ((eq? msg 'append) append)
            ((eq? msg 'fetch)  fetch)
            ((eq? msg 'serializer) serializer)))))

;;; Run the 'processor' function on the resources from the registry,
;;; where the resources are first sorted using 'sort' function and
;;; then their serializers are acquired in order before 'processor'
;;; execution.

(define (process-resources-registry processor sort)
  (define (serialize proc resources)
    (if (null? resources)
        proc
        (acquire-all (cdr resources) (((car resources) 'serializer) proc))))
  ((acquire-all processor (sort resources)) resources))

;;; Example shared resource

(define (number x)
  (lambda (msg)
    (let ((serializer (make-serializer)))
      (cond ((eq? msg 'value) x)
            ((eq? msg 'inc) (set! x (+ x 1)))
            ((eq? msg' serializer) serializer)))))

;;; Sorting for numbers

(define (sort-numbers numbers)
  )
  
(define (increase number)
  ((number 'inc)))
