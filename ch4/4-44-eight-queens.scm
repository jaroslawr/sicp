(define (require p)
  (if (not p)
      (amb)))

(define (cadr x)
  (car (cdr x)))

(define (a-member-of a-list)
  (if (null? a-list)
      (amb)
      (amb (car a-list)
           (a-member-of (cdr a-list)))))

(define (diff list-a list-b)
  (define (diff-iter list-a list-b result)
    (if (null? list-a)
        result
        (let ((current (car list-a)))
          (diff-iter (cdr list-a)
                     list-b
                     (if (member current list-b)
                         result
                         (cons current result))))))
  (diff-iter list-a list-b (list)))

(define (eight-queens)
  (define (queen row col)
    (list row col))
  (define (require-not-attacking-any queen-a queens)
    (if (null? queens)
        true
        (let ((queen-b (car queens)))
          (require (not (= (- (cadr queen-a) (car queen-a))
                           (- (cadr queen-b) (car queen-b)))))
          (require (not (= (+ (cadr queen-a) (car queen-a))
                           (+ (cadr queen-b) (car queen-b)))))
          (require-not-attacking-any queen-a (cdr queens)))))
  (define (generate-queens queens busy-cols size)
    (if (= size 8)
        queens
        (let ((new-queen
               (queen (+ size 1) (a-member-of (diff (list 1 2 3 4 5 6 7 8) busy-cols)))))
          (require-not-attacking-any new-queen queens)
          (generate-queens (cons new-queen queens)
                           (cons (cadr new-queen) busy-cols)
                           (+ size 1)))))
  (generate-queens (list) (list) 0))
