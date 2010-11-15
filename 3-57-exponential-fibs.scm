(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

;;; Without memoization, this stream is similar to such a traditional
;;; function:

(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else
         (+ (fib (- n 2)) (fib (- n 1))))))

;;; So to compute the n-th element of the stream, you have to compute
;;; the first n - 1 elements once and also separately the first n - 2
;;; elements. This yields the following recurrence relation:
;;;
;;; T(n) = T(n - 1) + T(n - 2) + O(1)
;;;
;;; Looking at some examples:
;;;
;;; T(0) = O(1)
;;; T(1) = O(1)
;;; T(2) = T(1) + T(0) + O(1)
;;; T(3) = T(2) + T(1) + O(1)
;;; ...
;;;
;;; Yields:
;;;
;;; T(n) = O(fib(n + 1))
;;; T(n) = O((Phi^(n+1))/sqrt(5)) ;;; See section 1.2.2 of the book
;;;
;;; With memoization, the elements of the fibonacci sequence are
;;; computed only once, yielding a much nicer relation:
;;;
;;; T(n) = T(n - 1) + O(1)
;;;
;;; T(n) = O(n)
;;;
;;; So it simply works in linear time in this case.
