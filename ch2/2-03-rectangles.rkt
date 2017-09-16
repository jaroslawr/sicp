(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point
   (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
   (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point point)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (dispaly ")")
  (newline))

(define (make-rectangle top-left dimensions)
  (cons top-left dimensions))

(define (top-left-rectangle rect)
  (car rect))

(define (dimensions-rectangle rect)
  (cdr rect))

(define (circumference-rectangle rect)
  (+
   (* 2 (+ (x-point (top-left-rectangle rect))
           (x-point (dimensions-rectangle rect))))
   (* 2 (+ (x-point (top-left-rectangle rect))
           (y-point (dimensions-rectangle rect))))))

(define (area-rectangle rect)
  (* (+ (x-point (top-left-rectangle rect))
        (x-point (dimensions-rectangle rect)))
     (+ (y-point (top-left-rectangle rect))
        (y-point (dimensions-rectangle rect)))))
