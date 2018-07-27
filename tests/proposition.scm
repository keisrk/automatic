(use-modules (automatic proposition)
             (srfi srfi-64))

(test-begin "proposition")
(test-equal "make-cls" (make-cls 3) #(#f #f #f))
(test-equal "make-singleton-cls" (make-singleton-cls 3 1 1) #(#f 1 #f))

(test-assert
 ;; conflicting clauses
 (cls-conflict? #(#f 1 #f 0) #(#f 1 #f 1)))

(test-assert
 ;; deducible clauses
 (cls-deducible? #(#f 1 #f 0) #(#f 1 #f 1)))


(test-equal
 "insert works"
 (insert cls<
         #(0 #f #f)
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

(test-equal
 "cls-negation works"
 (cls-negation  #(0 #f 1))
 '(#(#f #f 0)
   #(1 #f #f)))

(test-assert
 "dnf-negation-wrt and dnf-conjunction"
 (null?
  (dnf-conjunction
   '(#(#f 1 0)
     #(0 #f 0)
     #(1 #f #f))
   (dnf-negation-wrt
    3
    '(#(#f 1 0)
      #(0 #f 0)
      #(1 #f #f))))))

(test-equal
 "dnf-negation-wrt and dnf-union"
 (dnf-negation-wrt
  3
  '(#(#f 1 0)
    #(0 #f 0)
    #(1 #f #f)))
 '(#(0 #f 1) #(0 0 1)))

(test-equal
 "dnf-negation-wrt #nil case"
 (dnf-negation-wrt 4 #nil)
 '(#(#f #f #f #f)))

(test-equal
 "dnf-negation-wrt and dnf-union"
 '(#(#f #f #f))
 (dnf-simplify
  (dnf-union
   '(#(#f 1 0)
     #(0 #f 0)
     #(1 #f #f))
   (dnf-negation-wrt
    3
    '(#(#f 1 0)
      #(0 #f 0)
      #(1 #f #f))))))
;;    x1 -x2
;;-x0    -x2
;; x0
(display (l-cls->l-brnf '(1 0 1)))(newline)
(display (l-cls->l-brnf '(0 1 #f 0 1)))(newline)
(test-end)
