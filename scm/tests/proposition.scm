(use-modules (automatic utils)
             (automatic proposition)
             (ice-9 match)
             (rnrs lists)
             (srfi srfi-42)
             (srfi srfi-64))

(test-begin "proposition")
(define assign
  '(#(0 0 0)
    #(1 0 0)
    #(0 1 0)
    #(1 1 0)
    #(0 0 1)
    #(1 0 1)
    #(0 1 1)
    #(1 1 1)))

(define dnfs
  '((#( 1  1  0)#( 0  1 #f)#( 1  0 #f))
    (#( 0 #f  1)#(#f  1  0)#(#f  0 #f))
    (#(1  #f  0)#( 0  1  1)#(#f  0 #f))
    (#(#f 1  #f)#( 1 #f  1)#( 0  1  1)#( 0 #f 0)#( 1  1 #f))
    (#(#f 0  #f)#( 0 #f #f)#( 0  1  1)#( 0 0  #f))))

(define dnf-and-brnf   (map (lambda (dnf) (cons dnf (dnf->brnf dnf))) dnfs))
(define dnf-and-negdnf (map (lambda (dnf) (cons dnf (dnf-negation-wrt 3 dnf))) dnfs))
(define dnf-and-conjdnf (map (lambda (dnf) (cons dnf (dnf-conjunction dnf dnf))) dnfs))

(display"assignment")(newline)
(do-ec (: d-n-b dnf-and-brnf)
       (:let dnf (car d-n-b))
       (:let brnf (cdr d-n-b))
       (: asg assign)
       (format #t "~a" (cons (dnf-eval dnf asg) (brnf-eval brnf asg))))

(test-assert "assign"
             (for-all (lambda (result) result)
                      (list-ec (: d-n-b dnf-and-brnf)
                               (:let dnf (car d-n-b))
                               (:let brnf (cdr d-n-b))
                               (for-all (lambda (asg) (equal? (dnf-eval dnf asg) (brnf-eval brnf asg))) assign))))

(display"negation")(newline)
(do-ec (: d-n-nd dnf-and-negdnf)
       (:let dnf (car d-n-nd))
       (:let ndnf (cdr d-n-nd))
       (: asg assign)
       (format #t "~a\n" (cons (dnf-eval dnf asg) (dnf-eval ndnf asg))))

(test-assert "negation"
             (for-all (lambda (result) result)
                      (list-ec (: d-n-nd dnf-and-negdnf)
                               (:let dnf (car d-n-nd))
                               (:let ndnf (cdr d-n-nd))
                               (for-all (lambda (asg) (equal? (dnf-eval dnf asg) (not (dnf-eval ndnf asg)))) assign))))

(display"conjunction")(newline)
(do-ec (: d-n-cjdnf dnf-and-conjdnf)
       (:let dnf (car d-n-cjdnf))
       (:let cjd (cdr d-n-cjdnf))
       (: asg assign)
       (format #t "~a\n" (cons (dnf-eval dnf asg) (dnf-eval cjd asg))))

(test-assert "conjunction"
             (for-all (lambda (result) result)
                      (list-ec (: d-n-cjdnf dnf-and-conjdnf)
                               (:let dnf (car d-n-cjdnf))
                               (:let cjd (cdr d-n-cjdnf))
                               (for-all (lambda (asg) (equal? (dnf-eval dnf asg) (dnf-eval cjd asg))) assign))))


(test-equal "make-cls" (make-cls 3) #(#f #f #f))
(test-equal "make-singleton-cls" (make-singleton-cls 3 1 1) #(#f 1 #f))

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

(display "Result: dnf-negation-wrt and dnf-conjunction\n")
(display
(dnf-conjunction
   '(#(#f 1 0)
     #(0 #f 0)
     #(1 #f #f))
   (dnf-negation-wrt
    3
    '(#(#f 1 0)
      #(0 #f 0)
      #(1 #f #f))))
)(newline)

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
(test-equal
 "brnf-conjunction id"
 (brnf-multiplication
  '(#(0 0 0 0 0)) ;; = 1
  '(#(0 0 1 0 1)
    #(0 1 1 0 1)
    #(1 1 1 0 0)))
 '(#(0 0 1 0 1)
   #(0 1 1 0 1)
   #(1 1 1 0 0)))

(test-equal
 "term-substitution id"
 (term-substitution #(1 0 1) (lambda (i) #f))
 '(#(1 0 1)))

(test-equal
 "substitution id"
 (substitution '(#(0 0 1 0 1)
                 #(0 1 1 0 1)
                 #(1 1 1 0 0))
               (lambda (i) #f))
 '(#(0 0 1 0 1)
   #(0 1 1 0 1)
   #(1 1 1 0 0)))

(display (cls->brnf #(1 0 1)))(newline)
(display (cls->brnf #(0 1 #f 0 1)))(newline)
(display (dnf->brnf '(#(0 1 #f 0 1)
                      #(#f 0 1 0 1))))(newline)

(display (make-term 4)) (newline)
(display (make-singleton-term 4 1)) (newline)
(display (brnf-multiplication '(#(1 0 1)) #nil)) (newline)

(define afa-br-dlt
  '(((3 . #(1 1 1)) #(0 1 1 0) #(0 1 1 1) #(1 1 1 0) #(1 1 1 1)) ((2 . #(1 1 1)) #(0 0 0 1) #(0 0 1 1) #(0 1 0 1) #(0 1 1 1) #(1 0 0 1) #(1 0 1 1) #(1 1 0 1) #(1 1 1 1)) ((1 . #(1 1 1)) #(1 0 0 0) #(1 0 0 1) #(1 0 1 0) #(1 0 1 1)) ((0 . #(1 1 1)) #(1 1 0 0) #(1 1 0 1) #(1 1 1 0) #(1 1 1 1)) ((3 . #(0 1 1))) ((2 . #(0 1 1)) #(0 1 1 0) #(0 1 1 1) #(1 1 1 0) #(1 1 1 1)) ((1 . #(0 1 1)) #(0 0 0 0) #(0 0 1 0)) ((0 . #(0 1 1)) #(0 0 0 0) #(0 0 1 0) #(0 1 0 0) #(0 1 1 0)) ((3 . #(1 0 1)) #(0 0 1 0) #(0 0 1 1) #(0 1 1 0) #(0 1 1 1) #(1 0 1 0) #(1 0 1 1) #(1 1 1 0) #(1 1 1 1)) ((2 . #(1 0 1)) #(0 0 0 0) #(0 0 0 1)) ((1 . #(1 0 1)) #(1 0 1 0) #(1 0 1 1)) ((0 . #(1 0 1)) #(0 0 0 0) #(0 0 0 1)) ((3 . #(0 0 1)) #(1 1 1 0) #(1 1 1 1)) ((2 . #(0 0 1)) #(0 0 0 0) #(0 0 0 1) #(0 1 0 0) #(0 1 0 1) #(1 0 0 0) #(1 0 0 1) #(1 0 1 0) #(1 0 1 1) #(1 1 0 0) #(1 1 0 1) #(1 1 1 0) #(1 1 1 1)) ((1 . #(0 0 1)) #(0 0 0 0) #(0 0 0 1) #(0 0 1 0) #(0 0 1 1) #(1 0 0 0) #(1 0 0 1) #(1 1 1 0) #(1 1 1 1)) ((0 . #(0 0 1)) #(0 0 0 0) #(0 0 0 1)) ((3 . #(1 1 0)) #(1 0 1 0) #(1 0 1 1) #(1 1 1 0) #(1 1 1 1)) ((2 . #(1 1 0)) #(0 0 0 0) #(0 0 0 1)) ((1 . #(1 1 0)) #(0 0 0 0) #(0 0 0 1) #(0 1 1 0) #(0 1 1 1) #(1 0 0 0) #(1 0 0 1) #(1 1 1 0) #(1 1 1 1)) ((0 . #(1 1 0)) #(0 0 0 0) #(0 0 0 1) #(0 1 0 0) #(0 1 0 1) #(1 0 0 0) #(1 0 0 1) #(1 1 0 0) #(1 1 0 1)) ((3 . #(0 1 0)) #(0 0 0 0) #(0 0 0 1) #(0 0 1 0) #(0 0 1 1) #(0 1 0 0) #(0 1 0 1) #(0 1 1 0) #(0 1 1 1) #(1 0 0 0) #(1 0 0 1) #(1 0 1 0) #(1 0 1 1) #(1 1 0 0) #(1 1 0 1) #(1 1 1 0) #(1 1 1 1)) ((2 . #(0 1 0)) #(1 0 1 0) #(1 0 1 1) #(1 1 1 0) #(1 1 1 1)) ((1 . #(0 1 0)) #(0 0 1 0) #(0 0 1 1)) ((0 . #(0 1 0)) #(0 1 0 0) #(0 1 0 1)) ((3 . #(1 0 0)) #(0 0 0 1) #(0 0 1 1) #(0 1 0 1) #(0 1 1 1) #(1 0 0 1) #(1 0 1 1) #(1 1 0 1) #(1 1 1 1)) ((2 . #(1 0 0)) #(0 1 0 0) #(0 1 0 1)) ((1 . #(1 0 0)) #(0 0 0 0) #(0 0 0 1)) ((0 . #(1 0 0)) #(0 0 0 0) #(0 0 0 1)) ((3 . #(0 0 0)) #(0 1 1 0) #(0 1 1 1) #(1 1 1 0) #(1 1 1 1)) ((2 . #(0 0 0)) #(0 0 0 1) #(0 0 1 1) #(0 1 0 1) #(0 1 1 1) #(1 0 0 1) #(1 0 1 1) #(1 1 0 1) #(1 1 1 1)) ((1 . #(0 0 0)) #(1 0 0 0) #(1 0 0 1) #(1 0 1 0) #(1 0 1 1)) ((0 . #(0 0 0)) #(1 1 0 0) #(1 1 0 1) #(1 1 1 0) #(1 1 1 1)) ((init . #(0 0 0)) #(1 1 0) #(1 1 1)) ((init . #(1 0 0)) #(0 0 0)) ((init . #(0 1 0)) #(0 1 1)) ((init . #(1 1 0)) #(0 0 0) #(0 1 0) #(1 0 0) #(1 1 0)) ((init . #(0 0 1)) #(0 0 0) #(0 0 1) #(1 0 0) #(1 1 1)) ((init . #(1 0 1)) #(1 0 1)) ((init . #(0 1 1)) #(0 0 0) #(0 0 1) #(0 1 0) #(0 1 1)) ((init . #(1 1 1)) #(1 1 0) #(1 1 1)))
)
(define c #(1 1 1))
(define sgm
  (lambda (i)
    (match (assoc-ref afa-br-dlt (cons i c))
           (#f #f)
           (b b))))
(display "substitution")(newline)
(display (substitution '(#(0 1 0 1)
                 #(1 1 0 1)
                 #(1 1 0 0))
               sgm))(newline)
(test-end)
