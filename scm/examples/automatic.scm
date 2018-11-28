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

(define option-spec
  '((help     (single-char #\h) (value #f))
    (output   (single-char #\o) (value #t))
    (equation (single-char #\e) (value #t))))

(define help-content "\
Automatic fol prover [flag] [options]
Usage
$ auto dfa --equation \"(((x . -1) (y .  2)) . 1)\"
[flag]
  afa  Construct AFA
  dfa  Construct DFA
[options]
  -h, --help       Display this help
  -o, --output     Specify output filename
  -e, --equation   Provide an equation, e.g., \"(((x . -1) (y .  2)) . 1)\"
")

(define (unwrap-rawinput rawinput)
  "Before applying eval-string, wrap the raw string with quote."
  (let ((input (string-append "(quote " rawinput ")")))
    (eval-string input)))

(define (make-dot kind equation output)
  "Construct FA out of equation and write in dot format"
  (let* ((afa-flag     (equal? kind "afa"))
         (sigma        (equation->sigma    equation))
         (st8-dlt      (equation->st8-dlt  equation))
         (init-fin     (equation->init-fin equation))
         ;; AFA specific
         (be-st8       (if afa-flag (bin-enc-st8 (car st8-dlt) (car init-fin))))
         (be-init      (if afa-flag (assoc-ref be-st8 (car init-fin))))
         (be-final     (if afa-flag (assoc-ref be-st8 (cdr init-fin))))
         (be-dlt       (if afa-flag (bin-enc-dlt be-st8 (cdr st8-dlt))))
         (dlt-noninit  (if afa-flag (make-afa-dlt-noninit be-dlt be-st8)))
         (dlt-init     (if afa-flag (make-afa-dlt-init 'init dlt-noninit `(,be-final) sigma)))
         (afa-dlt      (if afa-flag (append dlt-noninit dlt-init)))
         (afa-brnf-dlt (if afa-flag (afa-dlt->afa-brnf-dlt afa-dlt)))
         (afa-st8-dlt  (if afa-flag (afa->st8-dlt sigma afa-brnf-dlt 'init)))
         (afa-reachable-dlt (if afa-flag (dlt-collect (cdr afa-st8-dlt)))))

    (if afa-flag
        ;; Handle AFA
        (begin
          (call-with-output-file output
            (lambda (port)
              (format-graph port 'init 'final (car afa-st8-dlt) afa-reachable-dlt afa-preamble)))
          (do-ec (: brnf (car afa-st8-dlt))
                 (draw-diagram
                  (list #:filename (string-append "resources/" (brnf->string brnf)) #:brnf brnf #:vars (vector-length (cdar be-st8))))))

        ;; Handle DFA
        (call-with-output-file output
          (lambda (port)
            (format-graph port
                          (car init-fin)
                          (cdr init-fin)
                          (car st8-dlt)
                          (dlt-collect (cdr st8-dlt))
                          dfa-preamble))))))

(define (main args)
  "Entry point"
  (let* ((options     (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (command     (option-ref options '() '())))

    (if help-wanted
        (display help-content)
        (match command
               ((or ("dfa")
                    ("afa")) (let* ((kind     (car command))
                                    (output   (option-ref options 'output (string-append kind ".dot")))
                                    (eq-raw   (option-ref options 'equation #f))
                                    (equation (if eq-raw (unwrap-rawinput eq-raw))))
                               (if (and equation (equation? equation))
                                   (make-dot kind equation output)
                                   (display "Invalid equation format\n"))))
               (_ (display help-content))))))
