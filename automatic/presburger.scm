(define-module (automatic presburger)
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
            assign-evaluate
            assign-init
            assign-succ
            make-assign-stream
            make-trans-stream
            make-equation->dfa
            equation->dfa
            ))

;; Variables are given as a list of symbols 'x 'y 'z ...
;;
;; Linear equation is an alist of var * int pairs, followed by a single
;; constant.
;; 
;; x + 2y + 3z = 7
;; coefficient: '((x . 1) (y . 2) (z . 3))
;; constant: 7

(define (distinct? l)
  ;; Check if the input list does not contain duplicating elements.
  (match l
         (#nil #t)
         ((x . xs) (and (not (member x xs)) (distinct? xs)))))

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

(define (convolution w)
  ;; (convolution '((x 1 1 1) (y 2 2 2) (z 3 3 3)))
  ;; = (#(x y z) #(1 2 3) #(1 2 3) #(1 2 3))
  (if (every null? w) #nil
      (cons (list->vector (map car w))
            (convolution (map cdr w)))))

(define (convolution-ordered w vars)
  ;; 2 inputs ((x . (1 1 1))(y . (2 2 2))(z . (3 3 3))) (y x z)
  ;; = (#(y x z) #(2 1 3) #(2 1 3) #(2 1 3))
  ;; Assume cdrs share the same length.
  (let ((w-ord (map (lambda (v) (assoc-ref w v)) vars)))
    (cons (list->vector vars) (convolution w-ord))))

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

(define (q-member? e q)
  ;;
  (member e (car q)))

(define (q-dedup-push! q e)
  ;;
  (if (q-member? e q) q (q-push! q e)))

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
  (stream-of (vector q c p)
           (a in (make-assign-stream coeffs))
           (c is (list->vector (map cdr a)))
           (p' is (assign-evaluate coeffs q a))
           (p is (if (even? p') (half p') #f))))

(define-stream (make-equation->dfa coeffs init-strm)
  ;;
  (stream-let loop ((todo-strm init-strm)
                    (st8s (make-q))
                    (trns (make-q)))
              (stream-match
               todo-strm
               (() (stream-of (cons (car st8s) (car trns))))
               ((head . strm)
                (match head
                       ((? integer? init)
                        (loop (make-trans-stream coeffs init)
                              (q-dedup-push! st8s init)
                              trns))
                       (#(q  _ #f)
                        (loop strm
                              st8s
                              (q-dedup-push! trns head)))

                       (#(q  _  p)
                        (if (q-member? p st8s)
                            (loop strm
                                  st8s
                                  (q-dedup-push! trns head))
                            (loop (stream-append strm (make-trans-stream coeffs p))
                                  (q-dedup-push! st8s p)
                                  (q-dedup-push! trns head)))))))))

(define (equation->dfa coeffs const)
  ;;
  (make-equation->dfa coeffs (stream-of const)))


(define (assign->alphabet assign vars)
  ;; : (symbol . int) alist -> symbol list -> int vector
  (list->vector (map (lambda (v) (assoc-ref assign v)) vars)))

