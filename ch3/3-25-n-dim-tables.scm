(define (make-table)
  (list '*table*))

(define (table? value)
  (and (list? value) (eq? (car value) '*table*)))

(define (assoc key records)
  (cond ((null? records) false)
        ((eq? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! keys-list value table)
  (define (append! key value)
    (set-cdr! table (cons (cons key value) (cdr table))))
  (let* ((key (car keys-list))
         (record (assoc key (cdr table))))
    (cond ((null? (cdr keys-list))
           (if record
               (set-cdr! record value)
               (append! key value))
           'ok)
          (else
           (if record
               (if (table? (cdr record))
                   (insert! (cdr keys-list) value (cdr record))
                   'invalid-insertion)
               (let ((subtable (make-table)))
                 (append! key subtable)
                 (insert! (cdr keys-list) value subtable)))))))

(define (lookup keys-list table)
  (let* ((key (car keys-list))
         (record (assoc key (cdr table))))
    (if record
        (if (null? (cdr keys-list))
            (cdr record)
            (if (table? (cdr record))
                (lookup (cdr keys-list) (cdr record))
                false))
        false)))
