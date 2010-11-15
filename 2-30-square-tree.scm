(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree)
         tree)
        ((list? tree)
         (append (list (square-tree (car tree))) (square-tree (cdr tree))))
        (else (square tree))))

(define (square-tree-2 tree)
  (map (lambda (node)
         (if (number? node)
             (square node)
             (square-tree-2 node)))
       tree))


