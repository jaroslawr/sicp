(rule (same ?x ?x))

(rule (can-replace? ?who ?whom)
      (and
       (or (and (job ?who ?job)
                (job ?whom ?job))
           (and (job ?coworker ?related-job)
                (job ?whom ?related-job)
                (job ?who ?job)
                (job ?coworker ?job)))
       (not (same ?who ?whom))))

;;; a)

(can-replace? ?who (Fect Cy D))

;;; b)

(and (salary ?person ?amount)
     (salary ?person2 ?amount2)
     (lisp-value > ?amount ?amount2)
     (can-replace? ?person2 ?person))
