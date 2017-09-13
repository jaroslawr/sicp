(define (flip-horiz painter)
  ((transform-painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)) painter))

(define (rotate180cc painter)
  ((transform-painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)) painter))

(define (rotate270cc painter)
  ((transform-painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)) painter))
