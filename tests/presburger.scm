(use-modules (automatic presburger)
             (srfi srfi-41)
             (srfi srfi-64))

(test-begin "presburger")
;; -2 x + 3 y - z = 7
(let* ((e '(((x . -2) (y .  3) (z . -1)) . 7)))
  (test-assert "equation" (equation? e)))

;; -7 x -13 y + 21 z = 37
(let* ((e '(((x . -7) (y .  -13) (z . 21)) . 37)))
  (test-assert "equation" (equation? e)))

;; -12 x 3 y + 5 z = -5
(let* ((e '(((x . -12) (y .  3) (z . 5)) . -5)))
  (test-assert "equation" (equation? e)))

(let ((coeffs '((x . 1) (y . 2) (z . 3)))
      (const 8))
  (stream-for-each
   display
   (make-trans-stream coeffs const))
  (newline))

(let ((coeffs '((x . 1) (y . 2) (z . 3)))
      (const 8))
  (display
   (stream-car
    (equation->dfa coeffs const)))
  (newline))

(test-end)
