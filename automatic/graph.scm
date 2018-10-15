(define-module (automatic graph)
  #:use-module (automatic utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-43)
  #:export (
            alphabet->string
            table->string
            dlt-collect
            format-graph
            ))

;; Input
;; #(0 a s x)
;; Output
;; Case 1: "0 a s x\n"
;; Case 2: "2,c,y,z"

;; Input
;; #(#(0 a s x)
;;   #(1 b t y)
;;   #(2 c u z))
;; Output
;; """
;; 0  a  s  x\n
;; 1  b  t  y\n
;; 2, c, u, z
;; """
(define (alphabet->string a)
  (match a
         (#f "#")
         ((? number? a) (number->string a))
         ((? symbol? s) (symbol->string s))))

(define (table->string table)
  (string-join
   (vector->list (vector-map
                  (lambda (i row)
                    (if (equal? (+ i 1) (vector-length table))
                        ;;"END"
                        (string-join (map alphabet->string (vector->list row)) ",")
                        ;;"MID"
                        (string-join (map alphabet->string (vector->list row)) " ")
                        )) table)) "\n"))

(define (dlt-collect dlt)
  ;; (orig * c) * dest alist to (orig * dest) * (c list) alist
  (fold (lambda (trans acc)
          (match trans
                 (((orig . c) . dest) (let* ((k (cons orig dest))
                                             (v (assoc-ref acc k)))
                                        (if v
                                            (acons k (cons c v) (assoc-remove! acc k))
                                            (acons k (list c) acc)))))) #nil dlt))

;; Emit graphviz's dot format string.
;; preamble := (lambda (port init final) (format port
;; "node[shape=circle];~&"))
(define (format-graph port init final st8 dlt preamble)
  (format port "digraph G {~&")
  (format port "rankdir=\"LR\";")
  (preamble port init final st8)
  (do-ec (: trans dlt)
         (match trans
                (((orig . dest) . c)
                 (format port "\"~a\"->\"~a\"[label=~s];~&" orig dest (table->string (transpose (list->vector c)))))))
  (format port "~&}~&~!"))
