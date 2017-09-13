(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v w)
  (make-vect
   (+ (xcor-vect v) (xcor-vect w))
   (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect
   (- (xcor-vect v) (xcor-vect w))
   (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v w)
  (make-vect
   (+ (xcor-vect v) (xcor-vect w))
   (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect
   (- (xcor-vect v) (xcor-vect w))
   (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (figure points)
  (define (iter points result)
    (if (null? points)
        result
        (let ((point-a (car points))
              (point-b (cadr points)))
          (iter (cddr points)
                (cons (make-segment
                 (make-vect (car point-a) (cadr point-a))
                 (make-vect (car point-b) (cadr point-b)))
                result)))))
  (lambda ()
    (segments->painter (iter points '()))))

(define contours
  (figure '((0.0 0.0) (0.0 0.99)
            (0.0 0.0) (0.99 0.0)
            (0.99 0.99) (0.0 0.99)
            (0.99 0.99) (0.99 0.0))))

(define xpainter
  (figure '((0.0 0.0) (0.99 0.99)
            (0.0 0.99) (0.99 0.0))))

(define parallelogram
  (figure '((0.5 0.0) (0.0 0.5)
            (0.5 0.0) (0.99 0.5)
            (0.99 0.5) (0.5 0.99)
            (0.5 0.99) (0.0 0.5))))

(define wave
  (figure '((0.2 0.0) (0.25 0.6)
            (0.4 0.0) (0.45 0.45)
            (0.6 0.0) (0.45 0.45)
            (0.8 0.0) (0.7 0.6)
            (0.7 0.6) (0.99 0.3)
            (0.99 0.5) (0.7 0.8)
            (0.7 0.8) (0.6 0.8)
            (0.6 0.8) (0.65 0.89)
            (0.65 0.89) (0.6 0.99)
            (0.4 0.99) (0.35 0.9)
            (0.35 0.9) (0.4 0.8)
            (0.4 0.8) (0.3 0.8)
            (0.3 0.8) (0.2 0.7)
            (0.2 0.7) (0.0 0.8)
            (0.25 0.6) (0.1 0.55)
            (0.1 0.55) (0.0 0.7))))
