(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

;;; 0
;;; => 0

(stream-ref x 5)

;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; => 5

(stream-ref x 7)

;;; 6
;;; 7
;;; => 7
