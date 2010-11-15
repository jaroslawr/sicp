;; (define (run-forever) (run-forever))
;;
;; (define (try p)
;;   (if (halts? p p)
;;       (run-forever)
;;       'halted))
;;
;; (try try)
;;
;; If (halts? try try) will return true, then this program will run
;; into an infinite loop, so the answer of halts? will not be correct.
;;
;; If it returns false, the program will terminate, so the answer also
;; will be false.
