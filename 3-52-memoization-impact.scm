(define (display-stream s)
  (stream-for-each display-line s))
  
(define sum 0)
;;; sum = 0

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;;; sum = 1
;;; seq = (1 promise)

(define y (stream-filter even? seq))
;;; sum = 6
;;; seq = (1 3 6 promise)
;;; y   = (6 promise)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
;;; sum = 10
;;; seq = (1 3 6 10)
;;; y   = (6 promsie)
;;; z   = (10 promise)

(stream-ref y 7)
;;; sum = 136
;;; seq = (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
;;; y   = (6 10 28 36 66 78 120 136 promise)
;;; z   = (10 promise)

(display-stream z)
;;; 10
;;; 15
;;; 45
;;; 55
;;; 105
;;; 120
;;; 190
;;; 210

;;; Without the memoization the results would be different, because
;;; after executing (define y (stream-filter ...)) seq would be (1
;;; promise), where sum would still be 6. The second filter would then
;;; start computing the consecutive seq values again, but now basing
;;; on the new sum value.
