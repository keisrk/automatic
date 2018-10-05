(define-module (automatic dfa)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (dfa-deterministic?
            dfa-preamble
            dfa-transition
            dfa-run))

;; DFA is a 5 tuple of
;; st8: int
;; init <- st8 / final <- st8
;; sigma: c list
;; dlt: (st8 * c) * st8
;; The use of srfi-9 Record seems somewhat overkill.

(define (dfa-deterministic? dlt)
  "test if dlt is deterministic"
  (fold (lambda (trans acc) (and #t acc)) #t dlt))

(define (dfa-preamble port init final st8)
  "preamble for DFA, required by (automatic graph)."
  (format port "edge[fontname=Courier];~&")
  (format port "node[shape=circle];~&")
  (format port "\"#entry#\"[shape=none label=\"\"];~&")
  (format port "\"~d\"[shape=doublecircle];~&" final)
  (format port "\"#entry#\"->\"~d\"~&" init))

(define (dfa-transition dlt q c)
  "DFA transition function, assuming deterministic"
  (assoc-ref dlt (cons q c)))

(define (dfa-run dlt q w)
  "DFA run function"
  (fold (lambda (c acc)
          (cons (dfa-transition dlt (car acc) c) acc)) (list q) w))
