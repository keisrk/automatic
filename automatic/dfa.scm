(define-module (automatic dfa)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<dfa>
            make-dfa
            dfa-sigma
            dfa-states
            dfa-init
            dfa-final
            dfa-delta
            dfa-transition
            dfa-run))

(define-record-type <dfa>
  ;; sigma: symbol -> {0, 1}
  ;; In stead of all mappings, it suffices to hold just symbols x, y, z, ...
  ;; states: int list
  ;; init, final: int
  ;; delta: ((int . int vector) . int) alist
  (make-dfa sigma states init final delta)
  dfa?
  (sigma     dfa-sigma)
  (states    dfa-states)
  (init      dfa-init)  
  (final     dfa-final)  
  (delta     dfa-delta))

(define (dfa-transition a q c)
  (assoc-ref (dfa-delta a) (cons q c)))

(define (dfa-run a q w)
  (fold (lambda (c acc)
          (cons (dfa-transition a (car acc) c) acc)) (list q) w))
