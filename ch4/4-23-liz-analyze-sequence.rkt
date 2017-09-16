(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

;;; For a sequence of just one expression, the version from the text
;;; will simply return a lambda that executes that expression with the
;;; given environment. The version by Liz will return a lambda that
;;; will then execute the inner 'execute-sequence' procedure, which
;;; will check if the rest of the expression list is null, and as it
;;; is so, it will compute the first expression and exit.

;;; For two expressions, the version from the text will simply return
;;; a lambda that computes them one after another. The version by Liz
;;; will again have to check if the rest of the list is null, then it
;;; will execute the first procedure, recursively call
;;; 'execute-sequence' a second time, again check if the rest of the
;;; list is null and only then it will compute the second expression
;;; and return.
