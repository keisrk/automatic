(use-modules (automatic)
             (automatic presburger)
             (automatic dfa)
             (automatic afa)
             (srfi srfi-64))

(test-begin "automatic")
;; -2 x + 3 y - z = 7
(define equation    '(((x . -2) (y .  3) (z . -1)) . 7))
(define sigma       (equation->sigma equation))
(define st8-dlt     (equation->st8-dlt  equation))
(define init-fin    (equation->init-fin equation))
(define be-st8      (bin-enc-st8 (car st8-dlt) (car init-fin)))
(define be-final    (assoc-ref be-st8 (cdr init-fin)))
(define be-dlt      (bin-enc-dlt be-st8 (cdr st8-dlt)))
(define dlt-noninit (make-afa-dlt-noninit be-dlt be-st8))
(define dlt-init    (make-afa-dlt-init 'init dlt-noninit `(,be-final) sigma (length (vars equation))))
(display dlt-noninit)(newline)
(display dlt-init)(newline)
(define afa-dlt (append dlt-noninit dlt-init))
(define afa-brnf-dlt (afa-dlt->afa-brnf-dlt afa-dlt))
(display afa-brnf-dlt)(newline)

(define afa-st8-dlt (afa->st8-dlt sigma afa-brnf-dlt 'init))
(test-end)
