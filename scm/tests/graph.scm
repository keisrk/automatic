(use-modules (automatic graph)
             (srfi srfi-64))

(test-begin "graph")

(define (preamble port init final st8)
  (format port "node[shape=circle];~&")
  (format port "\"#entry#\"[shape=none label=\"\"];~&")
  (format port "\"~d\"[shape=doublecircle];~&" final)
  (format port "\"#entry#\"->\"~d\"~&" init))

(define dlt '(((1 . 2) . (#(0 1 1 0) #(1 1 0 0) #(1 1 0 1)))))

(format-graph #t 0 1 '(1 2) dlt preamble)

(test-end)
