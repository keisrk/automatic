(define-module (automatic proposition)
  #:use-module (automatic utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-43)
  #:export (
            xor
            iff
            cls-eval
            dnf-eval
            term-eval
            brnf-eval

            ;; Methods for DNF 
            make-cls
            make-singleton-cls
            cls<
            cls-dnf-union
            cls-negation
            dnf-union
            dnf-conjunction
            dnf-negation-wrt

            ;; Methods for BRNF
            brnf->string
            make-term
            make-singleton-term
            term<
            term-multiplication
            term-brnf-xor
            term-substitution
            brnf-multiplication
            brnf-xor
            substitution

            ;; Methods for DNF -> BRNF conversion
            cls->brnf
            dnf->brnf
            ))

;; Methods for DNF 
;; lit := 0 | 1 | #f 
;; cls := lit vec
;; dnf := cls list
;; e.g., cls = #(0 1 #f 1)
;; cls = True iff cls = #(#f, #f, ..., #f)
;; dnf = {
;;        #(#f, #f), #(#f, 0), #(#f, 1),
;;        #(0, #f), #(0, 0), #(0, 1),
;;        #(1, #f), #(1, 0), #(1, 1)
;;        }
;; dnf = False iff dnf = {}
(define (xor l r) "xor" (match `(,l ,r) ((or (#t #f) (#f #t)) #t) (_ #f)))
(define (iff l r) "iff" (not (xor l r)))

(define (cls-eval cls assign)
  "assign[i] = 1 | 0"
  (vector-fold
   (lambda (i acc lit asg)
     (match lit
            (#f acc)
            (_ (and (equal? lit asg) acc))))
   #t cls assign))

(define (dnf-eval dnf assign)
  ""
  (fold
   (lambda (cls acc) (or acc (cls-eval cls assign)))
   #f
   dnf))

(define (term-eval term assign)
  "assign[i] = 1 | 0"
  (vector-fold
   (lambda (i acc lit asg)
     (match lit
            (0 acc)
            (1 (and (equal? 1 asg) acc))))
   #t term assign))

(define (brnf-eval brnf assign)
  ""
  (fold
   (lambda (term acc) (xor acc (term-eval term assign)))
   #f
   brnf))

(define (make-cls size)
  ;; size = 3 then #(#f #f #f)
  (make-vector size #f))

(define (make-singleton-cls size i lit)
  ;; size = 3; i = 1; lit = 1 then #(#f 1 #f)
  (let ((cls (make-cls size)))
    (vector-set! cls i lit)
    cls))

(define (lit< lit lit')
  ;; strict lex order over lit
  (match (cons lit lit')
         ((#f . #f) #f)
         ((#f . _) #t)
         ((_ . #f) #f)
         (_ (< lit lit'))))

(define (lit-negation lit)
  ;; negation literal
  (match lit
         (0 1)
         (1 0)
         (#f #f)))

(define (lit-conjunction lit lit')
  ;; lit /\ lit'
  ;; x0 /\ -x0 => BTM
  (match (cons lit lit')
         ((#f . _) lit')
         ((_ . #f) lit)
         ((0 . 0) 0)
         ((0 . 1) 'BTM)
         ((1 . 0) 'BTM)
         ((1 . 1) 1)))

(define (cls-conjunction cls cls')
  (let ((cls'' (vector-map (lambda (i lit lit') (lit-conjunction lit lit')) cls cls'))
        (btm?  (lambda (x) (equal? x 'BTM))))
    (if (vector-any btm? cls'') #f cls'')))

(define (s-cls< cls cls')
  ;; lex order over cls
  (vector-fold
   (lambda (i acc lit lit')
     (match acc
            ('equals (if (equal? lit lit') 'equals (lit< lit lit')))
            (x x)))
   'equals cls cls'))

(define (cls< cls cls')
  ;; lex order over cls
  (match (s-cls< cls cls')
         ('equals #f) ;; Strict odering
         (x x)))

(define (cls-dnf-union cls dnf)
  ;;
  (insert cls< cls dnf))

(define (cls-negation cls)
  ;; - #(#f 0 1) = { #(#f #f 0) #(#f 1 #f) }
  ;; - #(#f .. #f) = {}
  (vector-fold
   (lambda (i acc lit)
     (match lit
            (#f acc)
            (lit (cls-dnf-union
                  (make-singleton-cls (vector-length cls) i (lit-negation lit))
                  acc))))
   #nil cls))

(define (dnf-union dnf dnf')
  ;; (c1 c2) \/ (c1 c3) = (c1 c2 c3)
  (fold cls-dnf-union
        dnf' dnf))

(define (dnf-conjunction dnf dnf')
  ;; returns (dnf) /\ (dnf') in DNF form
  (fold-ec #nil
           (: cls dnf)
           (: cls' dnf')
           (:let cls'' (cls-conjunction cls cls'))
           (if cls'')
           cls''
           cls-dnf-union))

(define (dnf-negation-wrt vars dnf)
  ;; When vars = {x0 x1 x2} then (dnf-negation-wrt 3 dnf)
  ;; returns -(dnf) in DNF form
  (fold-ec (list (make-cls vars))
           (: cls dnf)
           (cls-negation cls)
           dnf-conjunction))

;; Methods for BRNF
;; term = 1 iff term = #(0 0 ... 0)
;; brnf = 0 iff brnf = #nil

(define (term->string term)
  (string-join (map number->string (vector->list term)) ""))

(define (brnf->string brnf)
  (match brnf
         ((? null? btm) "btm")
         ((? symbol? s) (symbol->string s))
         (_ (string-join (map (lambda (term) (term->string term)) brnf) "+"))))

(define (make-term size)
  ;; size = 3 then #(0 0 0)
  ;; #(0 .. 0): term = Top
  (make-vector size 0))

(define (make-brnf size)
  ;; size = 3 then (#(0 0 0))
  ;; (#(0 .. 0)): brnf = Top
  (list (make-vector size 0)))

(define (make-singleton-term size i)
  ;; size = 3; i = 1; #(0 1 0)
  (let ((term (make-term size)))
    (vector-set! term i 1)
    term))

(define (s-term< term term')
  ;;  lex order over terms
  (vector-fold
   (lambda (i acc v v')
     (match acc
            ('equals (if (equal? v v') 'equals (< v v')))
            (x x)))
   'equals term term'))

(define (term< term term')
  ;; lex order over term
  (match (s-term< term term')
         ('equals #f) ;; Strict odering
         (x x)))

(define (term-multiplication term term')
  ;; term * term
  (vector-map (lambda (i v v') (max v v')) term term'))

(define (term-brnf-xor term brnf)
  ;;
  (receive (same brnf')
           (partition (lambda (x) (equal? x term)) brnf)
           (if (null? same) (insert term< term brnf')
               (if (null? brnf') #nil brnf'))))

(define (brnf-multiplication brnf brnf')
  ;; brnf * brnf'
  (match (cons brnf brnf')
         ((#nil . _) #nil)
         ((_ . #nil) #nil)
         (_ (fold-ec #nil
                     (: term brnf)
                     (: term' brnf')
                     (term-multiplication term term')
                     term-brnf-xor))))

(define (brnf-xor brnf brnf')
  ;; brnf + brnf'
  (match (cons brnf brnf')
         ((#nil . #nil) #nil)
         ((#nil . _) brnf')
         ((_ . #nil) brnf)
         (_ (fold term-brnf-xor brnf brnf'))))

(define (term-substitution term sigma)
  ;; sigma :: int -> brnf | #f
  (vector-fold
   (lambda (i acc v)
     (case v
       ((0) acc)
       ((1) (match (sigma i)
                 (#f (brnf-multiplication (list (make-singleton-term (vector-length term) i)) acc))
                 (brnf (brnf-multiplication brnf acc))))
       ))
   (make-brnf (vector-length term)) term))

(define (substitution brnf sigma)
  ;;
  (fold (lambda (term acc) (brnf-xor (term-substitution term sigma) acc)) #nil brnf))

;; Methods for DNF -> BRNF conversion

(define (l-cls->l-brnf l-cls)
  ;;
  (match l-cls
         (#nil '(#nil))
         ((#f . l') (map (lambda (x) (cons 0 x)) (l-cls->l-brnf l')))
         ((1 . l') (map (lambda (x) (cons 1 x)) (l-cls->l-brnf l')))
         ((0 . l') (append (l-cls->l-brnf (cons #f l'))
                           (l-cls->l-brnf (cons 1 l'))))))

(define (cls->brnf cls)
  ;;
  (let* ((l-cls (vector->list cls))
         (l-brnf (l-cls->l-brnf l-cls)))
    (fold (lambda (l' acc) (term-brnf-xor (list->vector l') acc)) #nil l-brnf)))

(define (dnf->brnf dnf)
  ;;
  (match dnf
         (#nil #nil)
         ((cls . dnf') (let* ((brnf-l (cls->brnf cls))
                              (brnf-r (dnf->brnf dnf'))
                              (brnf-c (brnf-multiplication brnf-l brnf-r)))
                         (brnf-xor (brnf-xor brnf-l brnf-r) brnf-c)))))
