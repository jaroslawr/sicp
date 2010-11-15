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

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between low high)))
      (let ((k (an-integer-between low high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
