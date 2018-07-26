(use-modules (automatic proposition)
             (srfi srfi-64))

(test-begin "proposition")
(test-assert
 ;; conflicting clauses
 (cls-conflict? #(#f 1 #f 0) #(#f 1 #f 1)))

(test-assert
 ;; deducible clauses
 (cls-deducible? #(#f 1 #f 0) #(#f 1 #f 1)))


(test-equal
 "cls-dnf-place works"
 (cls-dnf-place #(0 #f #f)
                '(#(#f 1 0)
                  #(0 #f 0)
                  #(1 #f #f)))
 '(#(#f 1 0)
   #(0 #f #f)
   #(0 #f 0)
   #(1 #f #f)))

(test-equal
 "cls-dnf-union works"
 (cls-dnf-union  #(0 #f #f)
                '(#(#f 1 0)
                  #(0 #f 0)
                  #(1 #f #f)))
  '(#(#f #f #f)
    #(#f 1 0)
    #(0 #f 0)))

(test-end)
