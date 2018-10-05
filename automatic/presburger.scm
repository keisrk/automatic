(define-module (automatic presburger)
  #:use-module (automatic utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)           ;; List
  #:use-module (srfi srfi-9)           ;; define-record-type
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-41)          ;; Streams
  #:use-module (srfi srfi-42)          ;; Eager comprehensions
  #:export (
            equation?
            vars
            assign-evaluate
            assign-init
            assign-succ
            make-assign-stream
            make-trans-stream
            make-equation->st8-dlt
            equation->sigma
            equation->st8-dlt
            equation->init-fin
            equation->alphabet
            ))

;; Variables are given as a list of symbols 'x 'y 'z ...
;;
;; Linear equation is an alist of var * int pairs, followed by a single
;; constant.
;; 
;; x + 2y + 3z = 7
;; coefficient: '((x . 1) (y . 2) (z . 3))
;; constant: 7

(define (coefficients? coeffs)
  ;; Check if the input is a list of collectly formatted coefficients.
  (and (distinct? (map car coeffs))
       (every (lambda (x) (and (symbol? (car x))
                               (integer? (cdr x)))) coeffs)))

(define (equation? equation)
  ;; Check if the input is collectly formatted equation.
  (match equation
         (((? coefficients? coeffs) . (? integer? k)) #t)
         (_ #f)))

(define (vars equation)
  (map car (car equation)))

(define (assign-evaluate coeffs constant assign)
  ;; coeffs := '((x . 1) (y . 2) (z . 3))
  ;; assign := '((x . 0) (y . 2) (z . 2))
  ;; t_0 * v_0 + ... + t_n * v_n - c
  (- constant
     (fold-ec 0
              (: term coeffs)
              (match term
                     ((var . coef) (* coef (assoc-ref assign var))))
              +)))

(define (assign-init coeffs)
  ;; Make the initial value assignment for the given linear equation.
  (map (lambda (x)
         (cons (car x) 0)) coeffs))

(define (assign-end? assign)
  ;; True if all of the assignments are 1.
  (every (lambda (x) (equal? (cdr x) 1)) assign))

(define (assign-succ assign)
  ;; Successor for value assignment
  (match assign
         (#nil #nil)
         (((x . 0) . xs) (cons (cons x 1) xs))
         (((x . 1) . xs) (cons (cons x 0) (assign-succ xs)))))

(define (half n)
  ;; Guile's verbose way to perform 'n / 2'
  (euclidean-quotient n 2))

(define-stream (make-assign-stream coeffs)
  ;;
  (stream-unfold
   (lambda (x) x)                              ;; map
   (lambda (x) (not (equal? x 'END)))          ;; pred?
   (lambda (x) (if (assign-end? x) 'END
                   (assign-succ x)))           ;; gen
   (assign-init coeffs)))

(define-stream (make-trans-stream coeffs q)
  ;;
  (stream-of (cons (cons q c) p)
           (a in (make-assign-stream coeffs))
           (c is (list->vector (map cdr a)))
           (p' is (assign-evaluate coeffs q a))
           (p is (if (even? p') (half p') #f))))

(define-stream (make-equation->st8-dlt coeffs init-strm)
  ;;
  (stream-let loop ((todo-strm init-strm)
                    (st8 (make-q))
                    (dlt (make-q)))
              (stream-match
               todo-strm
               (() (stream-of (cons (car st8) (car dlt))))
               ((x . strm)
                (match x
                       ((? integer? init)
                        (loop (make-trans-stream coeffs init)
                              (q-dedup-push! st8 init)
                              dlt))
                       (((q . _) . #f)
                        (loop strm
                              st8
                              (q-dedup-push! dlt x)))

                       (((q . _) . p)
                        (if (q-member? p st8)
                            (loop strm
                                  st8
                                  (q-dedup-push! dlt x))
                            (loop (stream-append strm (make-trans-stream coeffs p))
                                  (q-dedup-push! st8 p)
                                  (q-dedup-push! dlt x)))))))))

(define (equation->sigma equation)
  ;;
  (match
   equation
   ((coeffs . const)
    (stream->list (stream-of
                   c
                   (a in (make-assign-stream coeffs))
                   (c is (list->vector (map cdr a))))))))

(define (equation->st8-dlt equation)
  ;;
  (match
   equation
   ((coeffs . const) (stream-match
                      (make-equation->st8-dlt coeffs (stream-of const))
                      ((x . _) x)))))

(define (equation->init-fin equation)
  ;; Conventionally the initial state is constant and final is 0.
  (match
   equation
   ((coeffs . const) (cons const 0))))

(define (equation->alphabet equation)
  ;;
  (match
   equation
   ((coeffs . const) (list->vector (map car coeffs)))))

