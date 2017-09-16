(define (split outer inner)
  (define (rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec painter (- n 1))))
          (outer painter (inner smaller smaller)))))
  rec)

(define right-split (split beside below))
(define up-split (split below beside))
