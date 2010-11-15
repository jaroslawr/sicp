(define (make-entry key value)
  (cons key value))

(define (entry-key entry)
  (car entry))

(define (entry-value entry)
  (cdr entry))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (lookup key node)
  (if (null? node)
      #f
      (let ((node-key (entry-key (entry node))))
        (cond
         ((equal? node-key key)
          (entry-value (entry node)))
         ((< key node-key)
          (lookup key (left-branch node)))
         ((> key node-key)
          (lookup key (right-branch node)))))))

(define sample-tree
  (make-tree (make-entry 9 'foo)
             (make-tree (make-entry 5 'bar)
                        (make-tree (make-entry 4 'baz) '() '())
                        (make-tree (make-entry 6 'bax) '() '()))
             (make-tree (make-entry 12 'omg)
                        (make-tree (make-entry 11 'lol) '() '())
                        (make-tree (make-entry 13 'zomg) '() '()))))
