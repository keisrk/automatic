(define-module (automatic proposition)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-43)
  #:export (
            insert

            make-cls
            make-singleton-cls

            cls<
            cls-conflict?
            cls-deducible?

            cls-dnf-union
            cls-negation
            dnf-simplify
            dnf-union
            dnf-conjunction
            dnf-negation-wrt

            l-cls->l-brnf
            ))

(define (insert e< e l)
  ;; e< :: 'a -> 'a -> bool
  ;; insertion sort
  (match l
         (#nil (list e))
         ((e' . l') (if (equal? e e') l
                            (if (e< e e') (cons e l)
                                (cons e' (insert e< e l')))))))

;; DNF Methods
;; lit := 0 | 1 | #f 
;; cls := lit vec
;; dnf := cls list
;; cls = #(0 1 #f 1)
;; cls = True iff cls = #(#f, #f, ..., #f)
;; dnf = {
;;        #(#f, #f), #(#f, 0), #(#f, 1),
;;        #(0, #f), #(0, 0), #(0, 1),
;;        #(1, #f), #(1, 0), #(1, 1)
;;        }
;; dnf = False iff dnf = {}

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

(define (lit-conflict? lit lit')
  (match (cons lit lit')
         ((0 . 1) #t)
         ((1 . 0) #t)
         (_ #f)))

(define (lit-negation lit)
  ;; negation literal
  (match lit
         (0 1)
         (1 0)
         (#f #f)))

(define (lit-conjunction lit lit')
  ;; lit /\ lit'
  (match (cons lit lit')
         ((#f . _) lit')
         ((_ . #f) lit)
         ((0 . _) 0)
         ((_ . 0) 0)
         (_ 1)))

(define (cls< cls cls')
  ;;  lex order over cls
  (vector-fold
   (lambda (i acc lit lit')
     (match acc
            ('init (if (equal? lit lit') 'init (lit< lit lit')))
            (x x)))
   'init cls cls'))

(define (cls-valid? cls)
  ;;
 (vector-every (lambda (lit) (equal? lit #f)) cls))

(define (cls-conflict? cls cls')
  ;; cls and cls' conflict when
  (vector-any lit-conflict? cls cls'))

(define (cls-deducible? cls cls')
  ;; cls and cls' are deducible when
  (vector-every (lambda (lit lit') (or (equal? lit lit')
                                       (lit-conflict? lit lit'))) cls cls'))

(define (cls-deduce cls cls')
  ;; cls'' is deduced from cls and cls' when
  (vector-map (lambda (i lit lit') (if (equal? lit lit') lit #f)) cls cls'))

(define (cls-dnf-union cls dnf)
  ;;
  (receive (ded-dnf dnf')
           (partition (lambda (x) (cls-deducible? x cls)) dnf)
           (if (null? ded-dnf) (insert cls< cls dnf)
               (let ((new-dnf (map (lambda (x) (cls-deduce x cls)) ded-dnf)))
                 (fold cls-dnf-union dnf' new-dnf)))))

(define (cls-negation cls)
  ;; - #(#f 0 1) = { #(#f #f 0) #(#f 1 #f) }
  ;;
  (vector-fold
   (lambda (i acc lit)
     (match lit
            (#f acc)
            (lit (insert cls<
                  (make-singleton-cls (vector-length cls) i (lit-negation lit))
                  acc))))
   #nil cls))

(define (dnf-simplify dnf)
  ;;
  (match dnf
         (((? cls-valid? min-cls) . _) (list min-cls))
         (_ dnf)))

(define (dnf-union dnf dnf')
  ;; (c1 c2) \/ (c1 c3) = (c1 c2 c3)
  (fold cls-dnf-union (dnf-simplify dnf') (dnf-simplify dnf)))

(define (dnf-conjunction dnf dnf')
  ;; returns (dnf) /\ (dnf') in DNF form
  (fold-ec #nil
           (: cls (dnf-simplify dnf))
           (: cls' (dnf-simplify dnf'))
           (if (not (cls-conflict? cls cls')))
           (vector-map (lambda (i lit lit') (lit-conjunction lit lit')) cls cls')
           cls-dnf-union))

(define (dnf-negation-wrt vars dnf)
  ;; When vars = {x0 x1 x2} then (dnf-negation-wrt 3 dnf)
  ;; returns -(dnf) in DNF form
  (fold-ec (list (make-cls vars))
           (: cls (dnf-simplify dnf))
           (cls-negation cls)
           dnf-conjunction))

(define (make-term size)
  ;; size = 3 then #(0 0 0)
  (make-vector size 0))

(define (make-singleton-term size i)
  ;; size = 3; i = 1; #(0 1 0)
  (let ((term (make-term size)))
    (vector-set! term i 1)
    term))

(define (term< term term')
  ;;  lex order over terms
  (vector-fold
   (lambda (i acc v v')
     (match acc
            ('init (if (equal? v v') 'init (< v v')))
            (x x)))
   'init term term'))

(define (l-cls->l-brnf l-cls)
  ;;
  (match l-cls
         (#nil '(#nil))
         ((#f . l') (map (lambda (x) (cons 0 x)) (l-cls->l-brnf l')))
         ((1 . l') (map (lambda (x) (cons 1 x)) (l-cls->l-brnf l')))
         ((0 . l') (append (l-cls->l-brnf (cons #f l'))
                           (l-cls->l-brnf (cons 1 l'))))))

(define (term-multplication term term')
  ;; term * term
  (vector-map (lambda (i v v') (max v v')) term term'))

(define (term-brnf-xor term brnf)
  ;;
  (receive (same brnf')
           (partition (lambda (x) (equal? x term)) brnf)
           (if (odd? (length same)) (insert term< term brnf')
               brnf')))
