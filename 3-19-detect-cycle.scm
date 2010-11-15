;;; Dokończyć rozwiązanie ze stałą pamięcią, ale kwadratowym czasem
;;; Spojrzeć na definicje cyklu
;;; Porównać z wersją bez ograniczenia pamięci
;;; Pomyśleć o innych sensownych sposobach destruktywnej modyfikacji listy

(define (make-cycle x)
  (set-cdr! (last-pair x) (cdr x))
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (contains list ref)
  (if (null? list)
      #f
      (or (eq? list ref) (contains (cdr list) ref))))

(define (detect-cycle a-list)
  (define (detect-cycle-rec head tail)
    (cond ((null? tail) #f)
          ((contains a-list tail) #t)
          (else
           (let ((next-tail (cdr tail)))
             (set-cdr! tail '())
             (set-cdr! head tail)
             (detect-cycle-rec (cdr head) next-tail)))))
  (let ((old-cdr (cdr a-list)))
    (set-cdr! a-list '())
    (detect-cycle-rec a-list old-cdr)))

;;;       +-----------------------+
;;;       |   - -    - -    - -   |
;;; z ->  +->| | |->| | |->| | |--+
;;;           - -    - -    - -
;;;           |      |      |
;;;          \|/    \|/    \|/
;;;           a      b      c

(define l (list 1 2 3 4))
(newline)
(newline)
(display (detect-cycle l))
(newline)
(display (detect-cycle z))
(newline)
