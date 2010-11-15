;;; Point A
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
            (0.1 0.55) (0.0 0.7)
            (0.45 0.82) (0.45 0.85)
            (0.45 0.82) (0.55 0.82)
            (0.55 0.82) (0.55 0.85))))

;;; Point B

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;;; Point C

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity flip-horiz identity)))
    (combine4 (corner-split painter n))))
