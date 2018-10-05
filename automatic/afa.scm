(define-module (automatic afa)
  #:use-module (automatic utils)
  #:use-module (automatic proposition)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41)          ;; Streams
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
            make-afa-dest-cls-init
            make-afa-dest-init
            make-afa-dlt-init

            ;; Methods
            afa-dlt->afa-brnf-dlt
            afa-brnf-transition
            afa-brnf-run

            afa->st8-dlt
            ))

;; Methods for numbers in  binary.
(define (binary-length n)
  "n: decimal number
Suppose state set has index from 0 to n. (binary-length n)
returns sufficient number of binarys to cover the index range, i.e. 
n := length- 1."
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

(define (make-afa-dest-cls-init afa-dlt-non-init be-final c vars)
  ;; When vars = {x0 x1 x2} then vars(size) = 3.
  ;; be-final: q0 = #(1 0 1 1), final state of DFA. 
  ;; assign: 0 -> 1, 1 -> 0, 2 -> 1, 3 -> 1
  ;; delta:  0 --c-> P0, 1 --c-> P1, 2 --c-> P2, 3 --c-> P3
  ;; c : one of the alphabet
  ;; (1 * P0) /\ (0 * P1) /\ (1 * P2) /\ (1 * P3)
  ;; Pn are all in DNF.
  (vector-fold
   (lambda (i acc assign)
     (match (assoc-ref afa-dlt-non-init (cons i c))
            (#f acc)
            (afa-dest
             (dnf-conjunction
              (case assign
                ((0) (dnf-negation-wrt vars afa-dest))
                ((1) afa-dest)) acc))))
   (list (make-cls vars)) be-final))

(define (make-afa-dest-init afa-dlt-non-init be-finalst8-bin c vars)
  ;; be-finalst8-bin: binary numbers list of finals.
  (fold-ec #nil
           (: be-final be-finalst8-bin)
           (make-afa-dest-cls-init afa-dlt-non-init be-final c vars)
           dnf-union))

(define (make-afa-dlt-init afa-init afa-dlt-non-init be-finalst8-bin sigma vars)
  ;;
  (list-ec (: c sigma)
           (:let dest (make-afa-dest-init afa-dlt-non-init be-finalst8-bin c vars))
           (cons (cons afa-init c) dest)))

(define (afa-dlt->afa-brnf-dlt afa-dlt)
  ;; 
  (map (lambda (afa-trans)
         (match afa-trans
                ((orig-c . dnf) (cons orig-c (dnf->brnf dnf)))))
       afa-dlt))

(define (afa-preamble port init final st8)
  "preamble for AFA, required by (automatic graph)."
  (format port "node[shape=none];~&")
  (format port "\"#entry#\"[shape=none label=\"\"];~&")
  (do-ec (: q st8)
         (format port "\"~a\"[image=\"~a.png\", label=\"\"];~&" q q))
  (format port "\"#entry#\"->\"~d\"~&" init))

;; Transition for AFA is just a substitution.
;; We assume each record of afa delta is in BRNF.
;; No matching is sent to Btm, #nil
(define (afa-brnf-transition afa-br-dlt brnf c)
  ;;
  (let ((sbst (lambda (orig)
                (match (assoc-ref afa-br-dlt (cons orig c))
                       (#f 'Not_Found)
                       (b b)))))
    (substitution brnf sbst)))

(define (afa-brnf-run afa-br-dlt brnf w)
  ;;
  (fold (lambda (c acc)
          (cons (afa-brnf-transition afa-br-dlt (car acc) c) acc)) (list brnf) w))

(define-stream (make-trans-stream sigma afa-brnf-dlt q)
  ;;
  (stream-of (cons (cons q c) p)
           (c in (list->stream sigma))
           (p is (afa-brnf-transition afa-brnf-dlt q c))))

(define-stream (make-st8-dlt sigma init afa-brnf-dlt)
  (stream-let loop ((todo-strm (make-trans-stream sigma afa-brnf-dlt init))
                    (st8 (make-q))
                    (dlt (make-q)))
              (stream-match
               todo-strm
               (() (stream-of (cons (car st8) (car dlt))))
               ((x . strm)
                (match x
                       (((q . _) . p)
                        (if (q-member? p st8)
                            (loop strm
                                  st8
                                  (q-dedup-push! dlt x))
                            (loop (stream-append strm (make-trans-stream sigma afa-brnf-dlt p))
                                  (q-dedup-push! st8 p)
                                  (q-dedup-push! dlt x)))))))))

(define (afa->st8-dlt sigma afa-brnf-dlt init)
  ;;
  (stream-match
   (make-st8-dlt sigma afa-brnf-dlt init)
   ((x . _) x)))
