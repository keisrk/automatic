(use-modules (automatic presburger)
             (automatic dfa)
             (automatic graph)
             (srfi srfi-64))

(test-begin "dfa")

(define dlt '(((init . H) . q1)
              ((q1 . E) . q2)
              ((q2 . Y) . final)))

(test-equal
 "dfa-run"
 (dfa-run dlt 'init '(H E Y))
 '(final q2 q1 init))

(define equation '(((x . -2) (y .  3) (z . -1)) . 7))
(define st8-dlt  (equation->st8-dlt  equation))
(define init-fin (equation->init-fin equation))
(define dlt (dlt-collect (cdr st8-dlt)))
(format-graph #t (car init-fin) (cdr init-fin) (car st8-dlt) dlt dfa-preamble)

(test-end)
