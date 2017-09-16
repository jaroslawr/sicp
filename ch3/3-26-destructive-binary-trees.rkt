(define (make-node key value left right)
  (cons (cons key value) (cons left right)))

(define (node-key node)
  (caar node))

(define (node-value node)
  (cdar node))

(define (node-left node)
  (cadr node))

(define (node-right node)
  (cddr node))

(define (make-table comparator)
  (cons comparator '()))

(define (table-comparator table)
  (car table))

(define (table-root table)
  (cdr table))

(define (set-table-root! table root)
  (set-cdr! table root))

(define (find-node key table)
  (let ((comparator (table-comparator table)))
    (define (traverse node parent)
      (if (null? node)
          (cons false parent)
          (let ((comparision (comparator key (node-key node))))
            (cond
             ((= comparision  0) (cons true node))
             ((= comparision -1) (traverse (node-left node) node))
             ((= comparision  1) (traverse (node-right node) node))
             (else (error "Invalid comparator function"))))))
    (traverse (table-root table) '())))

(define (insert! key value table)
  (define (update-existing-node node)
    (set-car! node value))
  (define (insert-root-node node)
    (set-table-root! table node))
  (define (insert-child-node parent node)
    (let* ((comparator (table-comparator table))
           (comparision (comparator key (node-key parent))))
      (cond
       ((= comparision -1) (set-car! (cdr parent) node))
       ((= comparision  1) (set-cdr! (cdr parent) node))
       (else (error "Invalid comparator function")))))
  (let* ((find-result (find-node key table))
         (found (car find-result))
         (node (cdr find-result)))
      (if found
        (update-existing-node node)
        (let ((new-node (make-node key value '() '())))
          (if (null? node)
              (insert-root-node new-node)
              (insert-child-node node new-node)))))
  'ok)

(define (lookup key table)
  (let* ((find-result (find-node key table))
         (found (car find-result))
         (node (cdr find-result)))
    (if found (node-value node) #f)))

(define (string-cmp a b)
  (cond ((string=? a b)  0)
        ((string>? a b)  1)
        (else           -1)))

(define t (make-table string-cmp))

(insert! "foo" 1 t)
(insert! "bar" 2 t)
(insert! "aaa" 3 t)
(insert! "ggg" 4 t)

(lookup "bar" t)
