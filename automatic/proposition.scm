(define-module (automatic proposition)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-43)
  #:export (
            cls-conflict?
            cls-deducible?
            cls-dnf-place
            cls-dnf-union
            dnf-union
            dnf-conjunction
            dnf-negation
            ))

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
  ;; size = 3 then #(#f, #f, #f)
  (make-vector size #f))

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

(define (cls-conflict? cls cls')
  ;;
  (vector-any lit-conflict? cls cls'))

(define (cls-deducible? cls cls')
  ;;
  (vector-every (lambda (lit lit') (or (equal? lit lit') (lit-conflict? lit lit'))) cls cls'))

(define (cls-deduce cls cls')
  ;;
  (vector-map (lambda (i lit lit') (if (equal? lit lit') lit #f)) cls cls'))

(define (cls-dnf-place cls dnf)
  ;; insertion sort
  (match dnf
         (#nil (list cls))
         ((cls' . dnf') (if (equal? cls cls') dnf
                            (if (cls< cls cls') (cons cls dnf)
                                (cons cls' (cls-dnf-place cls dnf')))))))

(define (cls-dnf-union cls dnf)
  ;;
  (receive (ded-dnf dnf')
           (partition (lambda (x) (cls-deducible? x cls)) dnf)
           (if (null? ded-dnf) (cls-dnf-place cls dnf)
               (let ((new-dnf (map (lambda (x) (cls-deduce x cls)) ded-dnf)))
                 (fold cls-dnf-union dnf' new-dnf)))))

(define (dnf-union dnf dnf')
  ;; (c1 c2) \/ (c1 c3) = (c1 c2 c3)
  (fold cls-dnf-union dnf' dnf))

(define (dnf-conjunction dnf dnf')
  ;; returns (dnf) /\ (dnf') in DNF form
  (fold-ec #nil
           (: cls dnf)
           (: cls' dnf')
           (if (not (cls-conflict? cls cls')))
           (vector-map (lambda (i lit lit') (lit-conjuntion lit lit')) cls cls')
           cls-dnf-union))

(define (dnf-negation dnf)
  ;; returns -(dnf) in DNF form
  (fold-ec '(#nil)
           (: cls dnf)
           (list (vector-map (lambda (i lit) (lit-negation lit)) cls))
           dnf-conjunction))
