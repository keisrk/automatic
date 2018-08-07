(define-module (automatic graph)
  #:use-module (automatic dfa)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-42)
  #:export (
            c->string
            cs->string
            dlt-collect
            format-graph
            )
  )

(define (c->string c)
  ;;
  (string-tabulate
   (lambda (i) (case (vector-ref c i)
                 ((0) #\0)
                 ((1) #\1)))
   (vector-length c)))

(define (cs->string cs)
  ;;
  (string-join (map cs->string cs) ","))

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
;; header := (lambda (port init final) (format port
;; "node[shape=circle];~&"))
(define (format-graph port init final dlt header cs->string)
  (format port "digraph G {~&")
  (header port init final)
  (do-ec (: trans dlt)
         (match trans
                (((orig . dest) . c)
                 (format port "\"~a\"->\"~a\"[label=~s];~&" orig dest (cs->string c)))))
  (format port "~&}~&~!")
)
