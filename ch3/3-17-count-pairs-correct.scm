(define (count-pairs x)
  (define (count-pairs-rec x visited)
    (if (or (not (pair? x)) (memq x visited))
        0
        (let ((new-visited (append visited (list x))))
          (+ (count-pairs-rec (car x) new-visited)
             (count-pairs-rec (cdr x) new-visited)
             1))))
  (count-pairs-rec x '()))
