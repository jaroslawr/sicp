(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a b)
  (cond ((and (= a 1) (= b 1))   1)
        ((or
          (and (= a 0) (= b 0))
          (and (= a 0) (= b 1))
          (and (= a 1) (= b 0))) 0)
        (else
         (error "Invalid signals" a b))))

(define (or-gate a1 a2 output)
  (let ((a1-inv  (make-wire))
        (a2-inv  (make-wire))
        (and-inv (make-wire)))
    (inverter a1 a1-inv)
    (inverter a2 a2-inv)
    (and-gate a1-inv a2-inv and-inv)
    (inverter and-inv output)
    'ok))

;;; The delay of this or-gat is 2 * inverter-delay + and-gate-delay
;;;
;;; a1  -|>o--+
;;;           |
;;;           +-+   +~~~.
;;;              == |    '--|>o-- output
;;;           +-+   +~~~'
;;;           |
;;; a2  -|>o--+
