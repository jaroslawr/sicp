(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;;
;;; Box-and-pointer diagrams for three-pair structures that will give
;;; answers: 3, 4, 7, the answer will never get computed
;;;

;;; 3
;;;
;;;   - -    - -    - -
;;;  | | |->| | |->| |/|
;;;   - -    - -    - -
;;;   |      |      |
;;;  \|/    \|/    \|/
;;;   a      b      c
;;;

;;; 4
;;;
;;;   - -    - -       - -
;;;  | | |->| | |->+->| |/|
;;;   - -    - -   |   - -
;;;   |      |     |   |
;;;   |     \|/    |  \|/
;;;   |      b     |   c
;;;   |            |
;;;   |            |
;;;   +------------+
;;;

;;; 7
;;;
;;;   - -    - -    - -
;;;  | | |->| | |->| |/|
;;;   - -    - -    - -
;;;   |/|\   |/|\
;;;   | |    | |
;;;   +-+    +-+
;;;

;;; Never gets computed
;;;
;;;   ----------------------+
;;;  |                      |
;;;  |   - -    - -    - -  |
;;;  +->| | |->| | |->| | |-+
;;;      - -    - -    - -
;;;      |      |      |
;;;     \|/    \|/    \|/
;;;      a      b      c
;;;
