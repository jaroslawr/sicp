(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval from to)
  (define (iter current result)
    (if (< current from)
        result
        (iter (- current 1) (cons current result))))
  (iter to '()))

(define (empty-board)
  '(()))

(define (adjoin-position row col board)
  (cons (list row col) board))

(define (find proc list)
  (cond ((null? list) '())
        ((proc (car list)) (car list))
        (else (find proc (cdr list)))))

(define (safe? column positions)
  (define (check? checking checked)
    (or
     (= (car checking) (car checked))
     (= (cadr checking) (cadr checked))
     (= (+ (car checking) (cadr checking))
        (+ (car checked)  (cadr checked)))
     (= (- (car checking) (cadr checking))
        (- (car checked)  (cadr checked)))))
  (define (check-all checking remaining)
    (cond
     ((null? remaining) #t)
     ((= (cadr (car remaining)) (cadr checking)) (check-all checking (cdr remaining)))
     ((and (not (check? checking (car remaining)))
           (check-all checking (cdr remaining))) #t)
     (else #f)))
  (check-all (find (lambda (queen) (= (cadr queen) column)) positions) positions))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
