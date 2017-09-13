(define (require p)
  (if (not p)
      (amb)))

(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
          (maybe-extend (list 'noun-phrase
                              noun-phrase
                              (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
          (maybe-extend (list 'verb-phrase
                              verb-phrase
                              (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-word word-list)
  (define (parse-word-rec word-list)
    (if (null? word-list)
        (amb)
        (amb (car word-list)
             (parse-word-rec (cdr word-list)))))
  (require (not (null? *unparsed*)))          
  (set! *unparsed* (cdr *unparsed*))
  (list (car word-list) (parse-word-rec (cdr word-list))))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

;;; Amb-Eval input:
;;; (parse '(a b c))
;;; 
;;; Starting a new problem 
;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun student)) (verb studies))
;;; 
;;; Amb-Eval input:
;;; try-again
;;; 
;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun student)) (verb lectures))
;;; 
;;; Amb-Eval input:
;;; try-again
;;; 
;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun student)) (verb eats))
;;; 
;;; Amb-Eval input:
;;; try-again
;;; 
;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun student)) (verb sleeps))
;;;
;;; ...
