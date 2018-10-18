(use-modules (automatic afa)
             (srfi srfi-64))

(test-begin "afa")

(test-equal
 "bin-enc-st8"
 (bin-enc-st8 '(q0 q1 q2 q3 q4) 'q0)
 '((q0 . #(0 0 0))
   (q1 . #(1 0 0))
   (q2 . #(0 1 0))
   (q3 . #(1 1 0))
   (q4 . #(0 0 1))))

(test-equal
 "bin-enc-st8 pt. 2"
 (bin-enc-st8 '(q0 q1 q2 q3 q4 q5 q6 q7) 'q3)
 '((q3 . #(0 0 0)) (q0 . #(1 0 0)) (q1 . #(0 1 0)) (q2 . #(1 1 0))
   (q4 . #(0 0 1)) (q5 . #(1 0 1)) (q6 . #(0 1 1)) (q7 . #(1 1 1))))

(display
;; (test-equal
;;  "bin-enc-dlt"
 (bin-enc-dlt (bin-enc-st8 '(q0 q1 q2 q3 q4) 'q0) '(((q0 . a) . q1) ((q0 . b) . q4)
                                                    ((q1 . a) . q2) ((q1 . b) . q0)
                                                    ((q2 . a) . q3) ((q2 . b) . q1)
                                                    ((q3 . a) . q4) ((q3 . b) . q2)
                                                    ((q4 . a) . q0) ((q4 . b) . q3)))
)
;;  '((b (#(0 0 1) . #(1 1 0))
;;       (#(1 1 0) . #(0 1 0))
;;       (#(0 1 0) . #(1 0 0))
;;       (#(1 0 0) . #(0 0 0))
;;       (#(0 0 0) . #(0 0 1)))
;;    (a (#(0 0 1) . #(0 0 0))
;;       (#(1 1 0) . #(0 0 1))
;;       (#(0 1 0) . #(1 1 0))
;;       (#(1 0 0) . #(0 1 0))
;;       (#(0 0 0) . #(1 0 0)))))

(newline)(display "make-afa-dlt-noninit")(newline)

(display
 (make-afa-dlt-noninit
  (bin-enc-dlt (bin-enc-st8 '(q0 q1 q2 q3 q4) 'q0) '(((q0 . a) . q1) ((q0 . b) . q4)
                                                     ((q1 . a) . q2) ((q1 . b) . q0)
                                                     ((q2 . a) . q3) ((q2 . b) . q1)
                                                     ((q3 . a) . q4) ((q3 . b) . q2)
                                                     ((q4 . a) . q0) ((q4 . b) . q3)))
  (bin-enc-st8 '(q0 q1 q2 q3 q4) 'q0)))


(newline)
(let* ((dlt-noninit (make-afa-dlt-noninit
             (bin-enc-dlt (bin-enc-st8 '(q0 q1 q2 q3 q4) 'q0) '(((q0 . a) . q1) ((q0 . b) . q4)
                                                                ((q1 . a) . q2) ((q1 . b) . q0)
                                                                ((q2 . a) . q3) ((q2 . b) . q1)
                                                                ((q3 . a) . q4) ((q3 . b) . q2)
                                                                ((q4 . a) . q0) ((q4 . b) . q3)))
             (bin-enc-st8 '(q0 q1 q2 q3 q4) 'q0)))
       (dlt-init (make-afa-dest-cls-init dlt-noninit #(0 1 1) 'a)))
  (display dlt-noninit)
  (newline)
  (display dlt-init)
  (newline))
(test-end)
