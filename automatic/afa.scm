(define-module (automatic afa)
  #:use-module (automatic proposition)
  #:use-module (automatic dfa)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-43)
  #:export (
            ;; Methods for numbers in  binary.
            binary-length
            decimal->binary
            binary-fill-zero
            binary-range

            ;; Methods for binary encoding.
            bin-enc-st8
            bin-enc-dlt

            ;; Methods for AFA construction.
            make-afa-dest-cls-noninit
            make-afa-dest-noninit
            make-afa-dlt-noninit
            ))

;; Methods for numbers in  binary.

(define (binary-length n)
  ;; n: decimal number
  ;; Suppose state set has index from 0 to n. (binary-length n) returns sufficient
  ;; number of binarys to cover the index range, i.e. n := length - 1.
  (if (or (equal? n 0)
          (equal? n 1))
      1
      (+ 1 (binary-length (euclidean-quotient n 2)))))

(define (decimal->binary n)
  ;; n: decimal number
  ;; 3 -> 11
  (let ((binary (euclidean-remainder n 2)))
    (if (or (equal? n 0)
            (equal? n 1))
        (list binary)
        (cons binary (decimal->binary (euclidean-quotient n 2))))))

(define (binary-fill-zero b n)
  ;; b: binary number 
  ;; n: decimal number
  ;; To adjust binary-lenght by filling 0
  (let ((diff (- n (length b))))
    (if (>= diff 0)
        (append b (make-list diff 0))
        #f)))

(define (binary-range m n)
  ;; m, n: decimal number
  ;; m: 0, n: 3
  ;; Enumerate binary numbers from m to n.
  (if (> m n) #nil
      (cons (binary-fill-zero (decimal->binary m) (binary-length n))
            (binary-range (+ 1 m) n))))

;; Methods for binary encoding.

(define (bin-enc-st8 dfa-st8 dfa-init)
  ;; (bin-enc-st8 '(q0 q1 q2 q3 q4 q5 q6 q7) 'q3)
  ;; ((q3 . #(0 0 0)) (q0 . #(1 0 0)) (q1 . #(0 1 0)) (q2 . #(1 1 0))
  ;;  (q4 . #(0 0 1)) (q5 . #(1 0 1)) (q6 . #(0 1 1)) (q7 . #(1 1 1)))
  (let* ((non-inits (filter (lambda (x) (not (equal? x dfa-init))) dfa-st8))
         (lhs (cons dfa-init non-inits))
         (rhs (binary-range 0 (- (length lhs) 1))))
    (map (lambda (l r) (cons l (list->vector r))) lhs rhs)))

(define (bin-enc-dlt be-st8 dfa-dlt)
  ;; (int * vec) * int alist to vec * (int vec * int vec) alist
  (fold (lambda (trans acc)
          (match trans
                 (((orig . k) . dest)
                  (let* ((v (cons (assoc-ref be-st8 orig)
                                  (assoc-ref be-st8 dest)))
                         (vs (assoc-ref acc k)))
                    (if vs
                        (acons k (cons v vs) (assoc-remove! acc k))
                        (acons k (list v) acc))) )))
        #nil dfa-dlt))

;; Methods for AFA construction.
(define (make-afa-dest-cls-noninit afa-orig be-orig be-dest)
  ;; afa-orig: int
  ;; be-orig, be-dest: binary number
  ;; be-dest[afa-orig] = 1 then returns cls
  ;; be-dest[afa-orig] = 0 then returns #f
  (if (equal? (vector-ref be-dest afa-orig) 0) #f be-orig))

(define (make-afa-dest-noninit afa-orig be-st8-bin be-orig-dest-al)
  ;; be-st8-bin: binary numbers list, i.e. (map cdr be-st8)
  ;; be-orig-dest-al:
  ;; returns DNF
  (fold-ec #nil
           (: be-orig be-st8-bin)
           (:let be-dest (assoc-ref be-orig-dest-al be-orig))
           (:let cls (if be-dest
                         (make-afa-dest-cls-noninit afa-orig be-orig be-dest)
                         #f))
           (if cls)
           cls
           cls-dnf-union))
 
(define (make-afa-dlt-noninit be-dlt be-st8)
  ;;
  (fold-ec #nil
           (: trans be-dlt)
           (: afa-orig (binary-length (- (length be-st8) 1)))
           (:let be-st8-bin (map cdr be-st8))
           (match trans
                  ((c . be-orig-dest-al)
                   (cons (cons afa-orig c)
                         (make-afa-dest-noninit afa-orig be-st8-bin be-orig-dest-al))))
           cons))

