(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (define (permute depth)
    (if (= depth 5)
        '((1) (2) (3) (4) (5))
        (flatmap
         (lambda (x)
           (map
            (lambda (n)
              (cons x n))
            (permute (+ depth 1))))
         (list 1 2 3 4 5))))
  (fold
   (lambda (permutation result)
     (let ((baker    (list-ref permutation 0))
           (cooper   (list-ref permutation 1))
           (fletcher (list-ref permutation 2))
           (miller   (list-ref permutation 3))
           (smith    (list-ref permutation 4)))
       (if
           (and (not (= baker 5))
                (not (= cooper 1))
                (not (= fletcher 5))
                (not (= fletcher 1))
                (> miller cooper)
                (not (= (abs (- fletcher cooper)) 1))
                (not (= (abs (- fletcher smith)) 1))
                (distinct? (list baker cooper fletcher miller smith)))
         (cons (list 'baker baker
                     'cooper cooper
                     'fletcher fletcher
                     'miller miller
                     'smith smith)
               result)
         result)))
   '()
   (permute 1)))
