;;; Lets encode the girls statements in terms of logical expressions
;;; encoded in Scheme:
;;;
;;; Ethel: (xor (= joan 2)  (= ethel 1))
;;; Joan:  (xor (= joan 3)  (= ethel 5))
;;; Kitty: (xor (= kitty 2) (= mary 4))
;;; Mary:  (xor (= mary 4)  (= betty 1)
;;; Betty: (xor (= kitty 2) (= betty 3))
;;;
;;; Knowing all those statements are true, we can build a "knowledge
;;; base" of "if...then" rules and then follow a procedure known as
;;; "resolution".
;;;
;;; If a girl takes some place, no other girl can take it. Based on
;;; this and on the fact that two clauses from the xor can't be true
;;; at once, we can write down what we know:
;;;
;;; If joan=2 then mary=4 and betty=3 and ethel!=1
;;; If joan=3 then kitty=2 and ethel!=5
;;; If kitty=2 then mary!=4 and betty!=3 and ethel=1
;;; If mary=4 then kitty=2 and betty!=1
;;; If kitty=2 then mary!=4 and betty!=3
;;; If ethel=1 then mary=4 and joan!=2
;;; If ethel=5 then joan!=3
;;; If mary=4 then kitty!=2 and betty!=1
;;; If betty=1 then joan=2 and mary!=4
;;; If betty=3 then ethel=5 and kitty!=2
;;;
;;; We can know remove the duplicated rules and order them a bit:
;;;
;;; If ethel=1 then mary=4 and joan!=2
;;; If betty=1 then joan=2 and mary!=4
;;; If joan=2 then mary=4 and betty=3 and ethel!=1
;;; If kitty=2 then mary!=4 and betty!=3 and ethel=1
;;; If kitty=2 then mary!=4 and betty!=3
;;; If betty=3 then ethel=5 and kitty!=2
;;; If joan=3 then kitty=2 and ethel!=5
;;; If mary=4 then kitty!=2 and betty!=1
;;; If ethel=5 then joan!=3
;;;
;;; Now, taking into consideration the logical rule:
;;; A -> B and B -> C <=> A -> B and C
;;;
;;; We have to look for contradictions in the rules, for example we
;;; can substitute:
;;;
;;; If joan=2 then mary=4 and betty=3 and ethel!=1
;;;
;;; Into:
;;;
;;; If betty=1 then joan=2 and mary!=4
;;;
;;; Resulting in:
;;;
;;; If betty=1 then joan=2 and mary!=4 and mary=4 and betty=3 and ethel!=1
;;;
;;; So betty can not be first, becuase it would lead to a
;;; contradiction. If Betty isn't first, we know from the knowledge
;;; base that Mary is fourth. If Mary is fourth, then Kitty isn't
;;; second. If Kitty isn't second, Betty is thrid. If Betty is thrid,
;;; Ethel is fifth. So we have:
;;;
;;; 1.
;;; 2.
;;; 3. Betty
;;; 4. Mary
;;; 5. Ethel
;;;
;;; If Mary is fourth, Kitty can not be second. The final result is:
;;;
;;; 1. Kitty
;;; 2. Joan
;;; 3. Betty
;;; 4. Mary
;;; 5. Ethel
