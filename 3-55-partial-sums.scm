(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (partial-sums stream)
  (define sum
    (cons-stream
     (stream-car stream)
     (add-streams sum (stream-cdr stream))))
  sum)

(define sums-stream (partial-sums integers))

;;; (1 (promise (add-streams (1 promise) (2 3 4 ...))))
;;; (1 3 (promise (add-streams (3 promise) (3 4 5 ...))))
