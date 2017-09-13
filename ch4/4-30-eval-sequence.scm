(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;;; a) In this for-each example. the side effects reside in an
;;; expression that is explictly "delayed" by the use of the
;;; lambda-expression regardless of the argument evaluation strategy
;;; chosen, and the evaluation moment is also explictly specified by
;;; the application of this lambda-expression. 

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

;;; b)
;;;
;;; Before changes:
;;;
;;; (p1 1) evaluates to (1 2) 
;;; (p2 1) evaluates to 1
;;;
;;; After changes:
;;;
;;; (p1 1) evaluates to (1 2) 
;;; (p2 1) evaluates to (1 2)
;;;
;;; The difference arises from the fact that variable references
;;; aren't forced, so before the changes the p2 procedure doesn't
;;; evaluate the "set!" expression at all.

;;; c) Nowhere during evaluating the 'begin' sequence of the for-each
;;; does the interpreter deal with thunks, so it doesn't matter if it
;;; uses eval or actual-value.

;;; d) As stated in the book lazy evaluation doesn't go well with side
;;; effects, so I would stick with the approach shown in the text,
;;; make the lazy argument evaluation optional as in exercise 4.31 and
;;; encourage programmers to avoid combining it with side-effects.
