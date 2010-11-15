;;; Assuming the requirements don't leave any side effects, their
;;; order doesn't affect the answer. It may have an significant impact
;;; on the time of execution only if checking the requirement takes a
;;; significant amount of time. In this case, the require's should be
;;; sorted by how large fraction of the possibilities they reject
;;; while considering how much time it costs to do the check.

(define (require p)
  (if (not p)
      (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
  
(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))

    ;;; This is a naive analysis, to be 100% to sure which order is
    ;;; optimal one would have to consider not how much the requires
    ;;; reject from the total space of possibilities, but how much do
    ;;; they reject of the remaining possibilities, considering some
    ;;; were already rejected by the previous require's. So one would
    ;;; have to consider all the 7! possible orderings of requires.
    
    ;;; There are 5^5 possibilties in total - 3125

    ;;; There are (4 + 3 + 2 + 1) * 4 ^ 3 possiblities where miller
    ;;; lives above cooper, so this rejects 2485 possibilities
    (require (> miller cooper))

    ;;; There are 17 * 4 ^ 3 cases where fletcher and cooper are on
    ;;; consecutive floors, this rejects 1008 possibilities
    (require (not (= (abs (- fletcher cooper)) 1)))

    ;;; There are 5^4 possibilities where baker is on the 5 floor, so
    ;;; this rejects 625 possibilities
    (require (not (= baker 5)))

    ;;; Just as with baker not being on the 5th floor
    (require (not (= cooper 1)))

    ;;; Just as with baker not being on the 5th floor
    (require (not (= fletcher 5)))

    ;;; Just as with baker not being on the 5th floor
    (require (not (= fletcher 1)))

    ;;; Only 120 of them are distinct (5!), so this rejects 3005
    ;;; possibilities, but also takes much much longer then the other
    ;;; checks.
    (require (distinct? (list baker cooper fletcher miller smith)))

    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))
