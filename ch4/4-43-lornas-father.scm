(define (require p)
  (if (not p)
      (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (cadr x)
  (car (cdr x)))

(define (lornas-father)
  (define (father) (amb 'mr-moore 'colonel-downing 'mr-hall 'sir-barnacle-hood 'dr-parker))
;;;                            yacht owner        daughters father
  (let ((mary-ann-moore (list 'dr-parker         'mr-moore))
        (gabrielle      (list 'sir-barnacle-hood (father)))
        (lorna          (list 'mr-moore          (father)))
        (rosalind       (list 'mr-hall           (father)))
        (melissa        (list 'colonel-downing   'sir-barnacle-hood)))
    (require
     (distinct?
      (list (cadr mary-ann-moore) (cadr gabrielle) (cadr lorna) (cadr rosalind) (cadr melissa))))
    (require (distinct? mary-ann-moore))
    (require (distinct? gabrielle))
    (require (distinct? lorna))
    (require (distinct? rosalind))
    (require (distinct? melissa))
    (let ((dr-parkers-daughter (amb mary-ann-moore gabrielle lorna rosalind melissa)))
      (require (eq? (cadr dr-parkers-daughter) 'dr-parker))
      (require (eq? (car dr-parkers-daughter) (cadr gabrielle)))
      (cadr lorna))))
