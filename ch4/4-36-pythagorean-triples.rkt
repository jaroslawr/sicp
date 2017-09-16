;; (define (pythagorean-triples)
;;   (let ((i (an-integer-starting-from 1)))
;;     (let ((j (an-integer-starting-from 1)))
;;       (let ((k (an-integer-starting-from 1)))
;;         (require (= (+ (* i i) (* j j)) (* k k)))
;;         (list i j k)))))
;;
;; This will not work too well, becuase it will infinitely loop with i
;; and j being fixed and equal to 1, while only k will get incremented
;; repeatedly.
;;
;; Instead, you can do something like this:
;;

(define (require p)
  (if (not p)
      (amb)))

(define (an-integer-between low high)
  (cond ((= low high)
         (amb))
        ((< low high)
         (amb low (an-integer-between (+ low 1) high)))
        (else
         (error "The range has to start with the lower number -- AN-INTEGER-BETWEEN"))))

(define (pythagorean-triples)
  (define (pythagorean-triples-sequence z)
    (define (pairs-with-sum-of-squares z)
      (let ((x (an-integer-between 1 z)))
        (let ((y (an-integer-between x z)))
          (require (= (+ (* x x) (* y y)) (* z z)))
          (list x y z))))
    (amb (pairs-with-sum-of-squares z)
         (pythagorean-triples-sequence (+ z 1))))
  (pythagorean-triples-sequence 1))
