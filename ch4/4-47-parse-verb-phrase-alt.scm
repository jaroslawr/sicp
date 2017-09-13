;;; This will not work, because it will never actually attempt to call
;;; parse-prepositional-phrase. (parse-verb-phrase) returns:
;;;
;;; (amb (parse-word verbs)
;;;        (list 'verb-phrase
;;;              (parse-verb-phrase)
;;;              (parse-prepositional-phrase)))
;;;
;;; If (parse-word verbs) now fails and backtracks, this expression
;;; gets evaluated:
;;;
;;; (list 'verb-phrase
;;;       (parse-verb-phrase)
;;;       (parse-prepositional-phrase))
;;;
;;; This first calls (parse-verb-phrase) again, leaving us in the
;;; situation we started in.
;;;
;;; Changing the order of arguments to amb in
;;; parse-prepositional-phrase similarly results in an infitite
;;; recursion.


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
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))
