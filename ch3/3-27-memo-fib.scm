(define (make-table)
  (list '*table*))

(define (table? value)
  (and (list? value) (eq? (car value) '*table*)))

(define (assoc key records)
  (cond ((null? records) false)
        ((eq? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let* ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))

(define (lookup key table)
  (let* ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(memo-fib 3)

;;;
;;; +-------------------------------------------------------------------------+
;;; |                  Global environment                                     |
;;; | fib -------------------------------------------------------+            |
;;; | memo-fib +                                                 |            |
;;; +-------------------------------------------------------------------------+
;;;            |                                                 |       /|\
;;;            |                                                \|/       |
;;;            |         +------------------------+          +-------+    |
;;;            |         | f: --------------------+--------> |   |   |    |
;;;            |         +------------------------+          +-------+    |
;;;            |                    /|\                        |   |      |
;;;            |                     |                        \|/  +------+
;;;            |                     |
;;;            |         +------------------------+       (define (fib n))
;;;            |         | table: (list '*table*) |         (cond ((= n 0) 0)
;;;           \|/        +------------------------+           ...
;;;         +-------+        /|\     /|\
;;;         |   |   |         |       |
;;;         +-------+         |    +------+
;;;           |   |           |    | x: 3 |
;;;           |   +-----------+    +------+
;;;          \|/
;;;
;;;    (lambda (x)
;;;      (let ((previously-computed-result
;;;            (lookup x table)))
;;;        ...
;;;

;;; (memo-fib 3) => (+ (memo-fib 2) (memo-fib 1))  [ Call 1 ]
;;; (memo-fib 2) => (+ (memo-fib 1) (memo-fib 0))  [ Call 2, left child of call 1 ]
;;; (memo-fib 1) => 1                              [ Call 3, left child of call 2, base case ]
;;; (memo-fib 0) => 0                              [ Call 4, right child of call 2, base case ]
;;; (memo-fib 1) => 1                              [ Call 5, right child of call 1, base case ]

;;; It works in linear time because it computes each fibonacci number
;;; only once, then uses the results stored in the table

;;; It wouldn't work as (memoize fib) because that would only memoize
;;; the first call and not the subsequent recursive calls
