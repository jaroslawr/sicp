(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require >= hsq ksq)
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

;;; The analysis follows from the number of possiblities that
;;; the combination of 'an-integer-between' calls produces.
;;;
;;; In the solution to exercise 4-35, if you want to find all the
;;; triples of the form:
;;; i^2 + j^2 = k^2
;;; For i, j and k all being less then or equal to some n, then you
;;; have to browse through n^3 possible solutions.
;;;
;;; Here, on the other hand, you only have to look at:
;;; n * ((n - 1) + (n - 2) + ... + 1) = n * (n / 2) = (n ^ 2) / 2
;;; possible solutions.
