;;; MZScheme

(require srfi/40)

(define (random-generator requests)
  (if (stream-null? requests)
      (stream)
      (let ((request (stream-car requests)))
        (cond
         ((eq? (car request) 'generate)
          (stream-cons (random)
                       (random-generator (stream-cdr requests))))
         ((eq? (car request) 'reset)
          (random-seed (cadr request))
          (random-generator (stream-cdr requests)))
         (else
          (error "Invalid request -- RANDOM-GENERATOR"))))))

(define randoms
  (random-generator
   (stream '(reset 0) '(generate) '(generate)
           '(reset 2) '(generate) '(generate)
           '(reset 0) '(generate) '(generate)
           '(reset 2) '(generate) '(generate))))

(newline)
(stream-for-each (lambda (random) (display random) (newline)) randoms)
