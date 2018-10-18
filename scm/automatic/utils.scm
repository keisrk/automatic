(define-module (automatic utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-43)
  #:export (
            insert
            distinct?
            q-member?
            q-dedup-push!
            table-ref
            transpose
            ))
;; Utilities

(define (insert e< e l)
  ;; e< :: 'a -> 'a -> bool
  ;; insertion sort
  (match l
         (#nil (list e))
         ((e' . l') (if (equal? e e') l
                            (if (e< e e') (cons e l)
                                (cons e' (insert e< e l')))))))

(define (distinct? l)
  ;; Check if the input list does not contain duplicating elements.
  (match l
         (#nil #t)
         ((x . xs) (and (not (member x xs)) (distinct? xs)))))

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

(define (q-member? e q)
  ;;
  (member e (car q)))

(define (q-dedup-push! q e)
  ;;
  (if (q-member? e q) q (q-push! q e)))

(define (table-ref table i j)
  (vector-ref (vector-ref table i) j))

;; Input
;; #(#(0 1 2)
;;   #(a b c)
;;   #(s t u)
;;   #(x y z))
;; Output
;; #(#(0 a s x)
;;   #(1 b t y)
;;   #(2 c y z))

(define (transpose table)
  (let ((rows    (vector-length (vector-ref table 0)))
        (columns (vector-length table)))
    (vector-unfold
     (lambda (i)
       (vector-unfold
        (lambda (j)
          (table-ref table j i))
        columns))
     rows)))

