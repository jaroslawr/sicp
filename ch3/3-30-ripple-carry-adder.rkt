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
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (cond ((or  (= a 1) (= b 1)) 1)
        ((and (= a 0) (= b 0)) 0)
        (else (error "Incorrect input signals" a b))))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a-ns b-ns y-ns carry-out)
  (define (iter a-ns b-ns y-ns carry-out)
    (if (null? (cdr a-ns))
        (let ((zero (make-wire)))
          (set-signal! zero 0)
          (full-adder (car a-ns) (car b-ns) zero (car y-ns) carry-out))
        (let ((carry-in (make-wire)))
          (iter (cdr a-ns) (cdr b-ns) (cdr y-ns) carry-in)
          (full-adder (car a-ns) (car b-ns) carry-in (car y-ns) carry-out))))
  (if (= (length a-ns) (length b-ns) (length-yns))
      (iter a-ns b-ns y-ns carry-out)
      (error "Input a, input b and output signal vectors are not of the same length"
             a-ns b-ns y-ns)))

;;; half-adder-delay         = max(and-gate-delay + inverter-delay, or-gate-delay) + and-gate-delay
;;; full-adder-delay         = 2 * half-adder-delay + or-gate-delay
;;; ripple-carry-adder-delay = n * full-adder-delay
;;; ripple-carry-adder-delay = n * [ 2 * (max(and-gate-delay + inverter-delay, or-gate-delay) + and-gate-delay) + or-gate-delay ]
