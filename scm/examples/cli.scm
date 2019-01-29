#!/usr/bin/guile  \
-e main -s
!#

(use-modules (automatic)
             (automatic graph)
             (automatic proposition)
             (automatic presburger)
             (automatic diagram)
             (automatic dfa)
             (automatic afa)
             (ice-9 eval-string)
             (ice-9 getopt-long)
             (ice-9 match)
             (srfi srfi-42))

(define dot-option-spec
  ;;  
  '((kind     (single-char #\k) (value #t))
    (output   (single-char #\o) (value #t))
    (equation (single-char #\e) (value #t))))

(define prop-option-spec
  '((op  (single-char #\p) (value #t))
    (lhs (single-char #\l) (value #t))    
    (rhs (single-char #\r) (value #t))))

(define help-content "\
Automatic fol prover [command] [options]
Usage
$ auto dot --equation \"(((x . -1) (y .  2)) . 1)\"
[command]
  dot  Construct DFA/AFA
  prop Construct Proposition
[options]
  -h, --help       Display this help
  -k, --kind       Either dfa or afa
  -o, --output     Specify output filename
  -e, --equation   Provide an equation, e.g., \"(((x . -1) (y .  2)) . 1)\"
")

(define (main args)
  "Entry point"
  (cond ((member "dot"  args) => command-make-dot)
        ((member "prop" args) => command-make-prop)
        (else (display help-content))))

(define (command-make-dot args)
  "Subcommand for dotfile."
  (let* ((options (getopt-long args dot-option-spec))
         (kind     (option-ref options 'kind #t))
         (output   (option-ref options 'output #t))
         (equation (option-ref options 'equation #t)))
    (make-dot kind (unwrap-rawinput equation) output)))

(define (command-make-prop args)
  "Subcommand for proposition handling."
  (let* ((options (getopt-long args prop-option-spec))
         (op  (option-ref options 'op #t))
         (lhs (option-ref options 'lhs #t))
         (rhs (option-ref options 'rhs #t)))
    (make-prop op (unwrap-rawinput lhs) (unwrap-rawinput rhs))))

(define (unwrap-rawinput rawinput)
  "Before applying eval-string, wrap the raw string with quote."
  (let ((input (string-append "(quote " rawinput ")")))
    (eval-string input)))

(define (make-dot kind equation output)
  "Construct FA out of equation and write in dot format"
  (let* ((sigma    (equation->sigma    equation))
         (st8-dlt  (equation->st8-dlt  equation))
         (init-fin (equation->init-fin equation)))

    (cond ((equal? kind "dfa")
             ;; Handle DFA
             (call-with-output-file output
               (lambda (port)
                 (format-graph port
                               (car init-fin)
                               (cdr init-fin)
                               (car st8-dlt)
                               (dlt-collect (cdr st8-dlt))
                               dfa-preamble))))
          ((equal? kind "afa")
            ;; Handle AFA
            (let* ((be-st8       (bin-enc-st8 (car st8-dlt) (car init-fin)))
                   (be-init      (assoc-ref be-st8 (car init-fin)))
                   (be-final     (assoc-ref be-st8 (cdr init-fin)))
                   (be-dlt       (bin-enc-dlt be-st8 (cdr st8-dlt)))
                   (dlt-noninit  (make-afa-dlt-noninit be-dlt be-st8))
                   (dlt-init     (make-afa-dlt-init 'init dlt-noninit `(,be-final) sigma))
                   (afa-dlt      (append dlt-noninit dlt-init))
                   (afa-brnf-dlt (afa-dlt->afa-brnf-dlt afa-dlt))
                   (afa-st8-dlt  (afa->st8-dlt sigma afa-brnf-dlt 'init))
                   (afa-reachable-dlt (dlt-collect (cdr afa-st8-dlt))))
               (begin
                 (call-with-output-file output
                   (lambda (port)
                     (format-graph port 'init 'final (car afa-st8-dlt) afa-reachable-dlt afa-preamble)))
                 (do-ec (: brnf (car afa-st8-dlt))
                    (draw-diagram
                      (list #:filename (string-append "resources/" (brnf->string brnf)) 
                            #:brnf brnf 
                            #:vars (vector-length (cdar be-st8))))))))
)))

(define (make-prop op lhs rhs)
  ;; 
  (let ((brnf 
          (match op
            ("and" (brnf-multiplication lhs rhs))
            ("or"  (brnf-disjunction    lhs rhs)))))
    (begin 
      (draw-diagram
        (list #:filename (string-append "resources/big/" (brnf->string brnf)) 
              #:brnf brnf 
              #:vars (vector-length (car brnf))))
      (display (brnf->string brnf)))))
