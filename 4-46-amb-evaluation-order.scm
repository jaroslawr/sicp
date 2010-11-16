;; If the arguments were evaluated right to left, the program would
;; run into an infinite recursive loop. For example parse-noun-phrase
;; would call parse-prepositional-prase, which then would call
;; parse-noun-phrase again and so on. The problem with that would be
;; that we would attempt to extend the parse at this point before
;; checking if it is already completed, as this check happens at the
;; bottom of the call stack in the parse-word function.
