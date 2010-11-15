(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval from to)
  (define (iter current result)
    (if (< current from)
        result
        (iter (- current 1) (cons current result))))
  (iter to '()))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (a-b-c-sums-to-d? a-b-c d)
  (= (+ (car a-b-c) (cadr a-b-c) (caddr a-b-c)) d))

(define (threes n)
  (flatmap
   (lambda (a)
     (flatmap
      (lambda (b)
        (map
         (lambda (c)
           (list a b c))
         (enumerate-interval (+ b 1) n)))
      (enumerate-interval (+ a 1) n)))
   (enumerate-interval 1 n)))

(define (sum-of-three n s)
  (filter (lambda (a-b-c) (a-b-c-sums-to-d? a-b-c s))
          (threes n)))
