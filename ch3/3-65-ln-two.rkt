(define (display-line x)
  (display x)
  (newline))

(define (display-stream s)
  (stream-for-each display-line s))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums stream)
  (define sum
    (cons-stream
     (stream-car stream)
     (add-streams sum (stream-cdr stream))))
  sum)

(define (ln-two-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-two-summands (+ n 1)))))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define ln-two (partial-sums (ln-two-summands 1)))
(define ln-two-euler (euler-transform ln-two))
(define ln-two-tableau (accelerated-sequence euler-transform ln-two))

;;; First digits of real expansion | .693147180559945
;;; (stream-ref ln-two 5)          | .6166666666666666
;;; (stream-ref ln-two-euler 5)    | .6928571428571428
;;; (stream-ref ln-two-tableau 5)  | .6931471806635636
