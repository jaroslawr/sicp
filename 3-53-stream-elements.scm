(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define s (cons-stream 1 (add-streams s s)))

;;; This is how the stream is constructed:
;;;
;;; Base case:
;;;
;;;    ---
;;;    1 (add-streams s s)
;;;
;;; First recursive "fold":
;;;
;;;    1 1 (add-streams s s)
;;;      1 (add-streams s s)
;;;  + -------------------------------------------
;;;    1 2 (+ (add-streams s s) (add-streams s s))
;;;
;;; Second recursive "fold":
;;;
;;;   1 2 1 2 (+ (add-streams s s) (add-streams s s))
;;;       1 2 (+ (add-streams s s) (add-streams s s))
;;;       1 2 (+ (add-streams s s) (add-streams s s))
;;;       1 2 (+ (add-streams s s) (add-streams s s))
;;; + -----------------------------------------------
;;;   1 2 4 8 (+ (add-streams s s) (add-streams s s)
;;;              (add-streams s s) (add-streams s s)
;;;              (add-streams s s) (add-streams s s)
;;;              (add-streams s s) (add-streams s s))
;;;
;;; [...]
