(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0
              (map (lambda (x)
                     (if (cons? x)
                         (count-leaves x)
                         1))
                   t)))
