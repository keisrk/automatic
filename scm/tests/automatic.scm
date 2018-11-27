(use-modules (automatic)
             (automatic graph)
             (automatic proposition)
             (automatic presburger)
             (automatic diagram)
             (automatic dfa)
             (automatic afa)
             (srfi srfi-42)
             (srfi srfi-64))

(test-begin "automatic")
;; -x + 2y  = 1
(define equation     '(((x . -1) (y .  2)) . 1))
(define sigma        (equation->sigma    equation))
(define st8-dlt      (equation->st8-dlt  equation))
(define init-fin     (equation->init-fin equation))
(call-with-output-file "resources/dfa.dot"
  (lambda (port)
    (format-graph port (car init-fin) (cdr init-fin) (car st8-dlt) (dlt-collect (cdr st8-dlt)) dfa-preamble)))

(display "##dlt\n")
(for-each (lambda (x) (format #t "~a\n" x)) (cdr st8-dlt))

(define be-st8       (bin-enc-st8 (car st8-dlt) (car init-fin)))
(define be-init     (assoc-ref be-st8 (car init-fin)))
(define be-final     (assoc-ref be-st8 (cdr init-fin)))
(define be-dlt       (bin-enc-dlt be-st8 (cdr st8-dlt)))
(define dlt-noninit  (make-afa-dlt-noninit be-dlt be-st8))
(define dlt-init     (make-afa-dlt-init 'init dlt-noninit `(,be-final) sigma))

(display "##dlt-noninit\n")
(for-each (lambda (x) (format #t "~a\n" x)) dlt-noninit)
(display "##dlt-init\n")
(for-each (lambda (x) (format #t "~a\n" x)) dlt-init)

(define afa-dlt      (append dlt-noninit dlt-init))
(define afa-brnf-dlt (afa-dlt->afa-brnf-dlt afa-dlt))
(define afa-st8-dlt  (afa->st8-dlt sigma afa-brnf-dlt 'init))
(define afa-reachable-dlt (dlt-collect (cdr afa-st8-dlt)))

(display "##afa-brnf-dlt\n")
(for-each (lambda (x) (format #t "~a\n" x)) afa-brnf-dlt)
(display "##afa-st8-dlt\n")
(for-each (lambda (x) (format #t "~a\n" x)) afa-st8-dlt)
(display "##afa-reachable-dlt\n")
(for-each (lambda (x) (format #t "~a\n" x)) afa-reachable-dlt)

(call-with-output-file "resources/afa.dot"
  (lambda (port)
    (format-graph port 'init 'final (car afa-st8-dlt) afa-reachable-dlt afa-preamble)))
(do-ec (: brnf (car afa-st8-dlt))
       (draw-diagram
        (list #:filename (string-append "resources/" (brnf->string brnf)) #:brnf brnf #:vars (vector-length (cdar be-st8)))))

(test-end)
