(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown procedure -- WIRE" m))))))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures)
         (call-each (cdr procedures))))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New value  ")
                 (display (get-signal wire)))))

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

(define the-agenda (make-agenda))

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 3)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
;;; sum 0  New-value = 0
(probe 'carry carry)
;;; carry 0  New-value = 0

(half-adder input-1 input-2 sum carry)
;;; ok

(set-signal! input-1 1)
;;; done
(propagate)
;;; sum 8  New-value = 1
;;; done

(set-signal! input-2 1)
;;; done
(propagate)
;;; carry 11 New-value = 1
;;; sum 16  New-value = 0

;;; With the current definition of accept-action-procedure!, for gate
;;; action procedures, we add to agenda the function to set the output
;;; value delay seconds in the future both as soon as the gate is
;;; connected and whenever the signal changes.
;;;
;;; If we define accept-action-procedure! as just:
;;;
;;; (define (accept-action-procedure! proc)
;;;   (set! action-procedures (cons proc action-procedures)))
;;;
;;; Then we only change the output value of the gate when the input
;;; values change, ignoring the initial input values completely.
;;;
;;; In this way in the half adder, we will not get the correct results
;;; until we set both signal values, beucase before that the inverter
;;; will not "work" at all.
;;;
;;; Thus, the execution of the example would look like this:
;;;
;;; (define input-1 (make-wire))
;;; (define input-2 (make-wire))
;;; (define sum (make-wire))
;;; (define carry (make-wire))
;;;
;;; (probe 'sum sum)
;;; (probe 'carry carry)
;;;
;;; (half-adder input-1 input-2 sum carry)
;;; ok
;;;
;;; (set-signal! input-1 1)
;;; done
;;; (propagate)
;;; carry 3 New-value = 0
;;; sum 8 New-value = 0
;;; done
;;;
;;; (set-signal! input-2 1)
;;; done
;;; (propagate)
;;; carry 11 New-value = 1
;;; sum 16 New-value = 1


