(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (list? structure)
        (total-weight structure)
        structure)))

(define (total-weight mobile)
  (if (null? mobile)
      0
      (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile)))))

(define (mobile-balanced? mobile)
  (define (branch-momentum branch path-length)
    (define (mobile-momentum mobile path-length)
      (+ (branch-momentum (left-branch mobile)  path-length)
         (branch-momentum (right-branch mobile) path-length)))
    (let ((structure (branch-structure branch)))
      (if (list? structure)
          (mobile-momentum structure (+ path-length (branch-length branch)))
          (* (branch-weight branch) (+ path-length (branch-length branch))))))
  (= (branch-momentum (left-branch mobile) 0)
     (branch-momentum (right-branch mobile) 0)))
