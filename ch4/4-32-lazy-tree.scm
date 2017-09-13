;;; As it was said in the footnotes, one can use the fact that both
;;; the car and the cdr are delayed, to build more sophisticated lazy
;;; data structures upon lazy conses. A simple lazy tree
;;; implementation may look like this:

(define (cons x y)
  (lambda (m) (m x y))))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define (lazy-tree-node data left right)
  (cons data (cons left right)))

(define (lazy-tree-node-data node)
  (car node))

(define (lazy-tree-node-left node)
  (car (cdr node)))

(define (lazy-tree-node-right node)
  (cdr (cdr node)))

(define (scale-tree node factor)
  (lazy-tree-node
   (* factor (lazy-tree-node-data node))
   (scale-tree (lazy-tree-node-left node) factor)
   (scale-tree (lazy-tree-node-right node) factor)))

(define infinite-tree
  (lazy-tree-node
   1
   (scale-tree infinite-tree 2)
   (scale-tree infinite-tree 3)))
