(define-module (automatic diagram)
  #:use-module (cairo)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-43)
  #:export (
            ;; Constants
            magenta
            cyan
            orange
            light-green

            ;; Methods for 2D Point            
            point-plus
            point-mult

            ;; Methods of cairo-XXX context
            <diagram>
            diagram-surf
            diagram-context
            new-diagram
            diagram-init
            diagram-write-png
            diagram-rightbottom
            diagram-center
            diagram-rotate
            diagram-color
            diagram-path
            diagram-text
            diagram-draw
            diagram-clip
            diagram-mode
            diagram-brnf
            diagram-stroke
            diagram-fill
            draw-diagram
            ))

;; Constants
(define pi 3.141592654)
(define magenta '(1.0 0.0 1.0))
(define cyan '(0.0 1.0 1.0))
(define orange '(1.0 0.5 0.0))
(define light-green '(0.0 0.5 0.0))

;; Methods for 2D Point
(define (point-plus p1 p2)
  ;; Addition
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))

(define (point-add-x c p)
  ;; Scalar addition for x
  (cons (+ c (car p)) (cdr p)))

(define (point-add-y c p)
  ;; Scalar addition for y
  (cons (car p) (+ c (cdr p))))

(define (point-mult k p)
  ;; Scalar multiplication
  (cons (* k (car p)) (* k (cdr p))))

;; On input brnf, outputs png file.

;; Methods of cairo-XXX context
;; new-diagram w h -> <diagram>
;; init-diagram <diagram> -> UNDEFINED

(define-record-type <diagram>
  ;; 
  (make-diagram width height radius font-size span vars surf context)
  diagram?
  (width     diagram-width)
  (height    diagram-height)
  (radius    diagram-radius)
  (font-size diagram-font-size)
  (span      diagram-span)
  (vars      diagram-vars         set-diagram-vars!)
  (surf      diagram-surf         set-diagram-surf!)
  (context   diagram-context      set-diagram-context!))

(define (new-diagram width height radius font-size span)
  ;; Constructor
  ;; BRNF yet to be loaded. vars = 0
  ;; Surf yet to be created. surf = #nil
  ;; Context yet to be created. context = #nil
  (make-diagram width height radius font-size span 0 #nil #nil))

(define (diagram-init diagram vars)
  ;; Assign newly created surf & context.
  (let* ((surf (cairo-image-surface-create
                'argb32
                (diagram-width diagram)
                (diagram-height diagram)))
         (context (cairo-create surf)))
    (cairo-set-font-size context (diagram-font-size diagram))
    (set-diagram-vars! diagram vars)
    (set-diagram-surf! diagram surf)
    (set-diagram-context! diagram context)
))

(define (diagram-write-png diagram filename)
  ;; filename "diagram" may yield "diagram.png".
  (cairo-surface-write-to-png (diagram-surf diagram)
                              (string-append filename ".png")))

(define (diagram-rightbottom diagram)
  ;; return right bottom point
  (cons (diagram-width diagram)
        (diagram-height diagram)))

(define (diagram-center diagram)
  ;; return center point
  (point-mult 0.5 (diagram-rightbottom diagram)))

(define (diagram-rotate diagram i)
  ;; Unit length
  ;; Given vector length len, (x, y) <- (diagram-rotate diagram i))
  ;; (len * x, len *y)
  (let* ((ratio (/ (* 2 pi) (diagram-vars diagram)))
         (x (cos (+ (* pi -0.5) (* i ratio))))
         (y (sin (+ (* pi -0.5) (* i ratio)))))
    (cons x y)))

(define (diagram-factor diagram)
  ;; Proportion to the number of variables
  (let ((vars (diagram-vars diagram))
        (center (diagram-center diagram))
        (radius (diagram-radius diagram)))
    (if (< 0.8 (log10 vars)) 0.8 (log10 vars))))

;; Cairo
;; 1. Source :: Specify color
;; 2. Path :: Specify shape
;; 3. Draw :: Specify fill or stroke

(define (diagram-color diagram c)
  ;; Prepare source with a color.
  (match c
         ('black (cairo-set-source-rgb (diagram-context diagram) 0.0 0.0 0.0))
         ('white (cairo-set-source-rgb (diagram-context diagram) 1.0 1.0 1.0))
         ('R (cairo-set-source-rgb (diagram-context diagram) 1.0 0 0))
         ('G (cairo-set-source-rgb (diagram-context diagram) 0 1.0 0))
         ('B (cairo-set-source-rgb (diagram-context diagram) 0 0 1.0))
         ((r g b) (cairo-set-source-rgb (diagram-context diagram) r g b))))

(define (diagram-bound diagram)
  (let ((span (diagram-span diagram))
        (center (diagram-center diagram))
        (radius (diagram-radius diagram))
        (factor (diagram-factor diagram)))
    (list 'circle center (+ (* factor radius) (* 2 span) radius))))

(define (diagram-path diagram path)
  ;; Prepare source with a path.
  (match path
         (('circle (x . y) r)
          (cairo-new-sub-path (diagram-context diagram))
          (cairo-arc (diagram-context diagram) x y r 0.0 (* 2 pi)))
         (('rectangle (x . y) (w . h))
          (cairo-rectangle (diagram-context diagram) x y w h))))

(define (diagram-text diagram p s)
  ;; Prepare source with a text path.
  (cairo-move-to (diagram-context diagram)
                 (- (car p) (* (* (string-length s) 0.5) (* (diagram-font-size diagram) 0.5)))
                 (+ (cdr p) (* (diagram-font-size diagram) 0.5)))
  (cairo-text-path (diagram-context diagram) s))

(define (diagram-draw diagram mode)
  ;; Given diagram-context's source and path, draw.
  (match mode
         ('fill (cairo-fill (diagram-context diagram)))
         ('stroke (cairo-stroke (diagram-context diagram)))))

(define (diagram-clip diagram path-seq)
  ;; cairo-clip consumes active path and restricts the drawing area. New clip
  ;; area is the intersection of the previous clip area and the input path.
  ;; (diagram-clip diagram #nil) resets the clip area.
  (cairo-reset-clip (diagram-context diagram))
  (do-ec (: p path-seq)
         (begin
           (diagram-path diagram p)
           (cairo-clip (diagram-context diagram))
           ))
  )

;; Cairo rendering operation
;; Following Source-Path-Draw cycles are XOR-ed or overwritten.
(define (diagram-mode diagram mode)
  ;; mode := 'over | 'xor | 'dest-over
  (cairo-set-operator (diagram-context diagram) mode))

(define (diagram-var diagram var)
  ;; var : int
  ;; Only output path instruction
  (let* (;;(vars (diagram-vars diagram))
         (center (diagram-center diagram))
         (radius (diagram-radius diagram))
         ;;(factor (if (< 0.8 (log10 vars)) 0.8 (log10 vars)))
         (factor (diagram-factor diagram))
         (length (* radius factor)))
    (list 'circle
          (point-plus center
                      (point-mult length (diagram-rotate diagram var))) radius)))

(define (diagram-label diagram var)
  (let* ((center (diagram-center diagram))
         (radius (diagram-radius diagram))
         (factor (diagram-factor diagram))
         (span (diagram-span diagram))
         (length (+ (* radius factor) (+ radius span))))
    (diagram-text diagram (point-plus center (point-mult length (diagram-rotate diagram var)))
                (format #f "~a~d" 'X var))))

(define (diagram-term diagram term)
  ;; term : {0, 1}^*, boolean vector
  ;; Only handle Path step
  (let* ((endpoint (diagram-rightbottom diagram))
         (path-seq
          (vector-fold (lambda (i acc v)
                         (case v
                           ((0) acc)
                           ((1) (cons (diagram-var diagram i) acc))))
                       #nil term)
          ;;          (list-ec (: var term)
          ;;                   (diagram-var diagram var))
          )
         (fillall  (diagram-bound diagram)))
    (diagram-clip diagram path-seq)
    (diagram-path diagram fillall)))

(define (diagram-brnf diagram brnf)
  ;; brnf : term list
  ;; Set mode to 'XOR'
  (diagram-mode diagram 'xor)
  ;; Process Path and Draw steps
  (for-each (lambda (term)
              (diagram-term diagram term)
              (diagram-draw diagram 'fill)) brnf)
  (diagram-clip diagram #nil)
  ;; Unset mode to default
  (diagram-mode diagram 'over))

(define (diagram-stroke diagram)
  ;;
  (do-ec (: i (diagram-vars diagram))
         (begin
           (diagram-path diagram (diagram-var diagram i))))
  (diagram-draw diagram 'stroke))

(define (diagram-fill diagram)
  (do-ec (: i (diagram-vars diagram))
         (begin
           (diagram-label diagram i)))
  (diagram-draw diagram 'fill))
;; 1. Draw diagram (Clipping & XOR mode)
;; 2. Draw background (DEST_OVER mode)
;; 3. Draw text & strokes (OVER mode)

(define (draw-diagram arg)
  ;;'(#:width w #:height h #:radius r #:font-size f #:span s
  ;;  #:bg-color bc #:fill-color fc #:stroke-color sc
  ;;  #:filename fn
  ;;  #:brnf b #:vars v)
  (let-keywords
   arg #f
   (;; Canvas setting
    (width  170)
    (height 170)
    (radius 50)
    (font-size 10)
    (span 8)
    ;; Color setting
    (bg-color 'white)
    (fill-color cyan)
    (stroke-color 'black)
    ;; Filename
    (filename "diagram")
    ;; BRNF
    (brnf '(#nil))
    (vars 3))
   (let ((diagram (new-diagram width height radius font-size span)))
     ;; Initialize
     (diagram-init diagram vars)
     ;; Brnf
     ;;;; Set fill color
     (diagram-color diagram fill-color)
     ;;;; Draw BRNF
     (if brnf (diagram-brnf diagram brnf))
     ;; Back Ground
     ;;;; Set mode to 'DEST_OVER'
     (diagram-mode diagram 'dest-over)
     ;;;; Set background color
     (diagram-color diagram bg-color)
     (diagram-path diagram (diagram-bound diagram))
     (diagram-draw diagram 'fill)
     ;; Lables
     ;;;; Set mode to 'OVER'
     (diagram-mode diagram 'over)
     ;;;; Set stroke color
     (diagram-color diagram stroke-color)
     ;;;; Stroke
     (diagram-stroke diagram)
     ;;;; Fill
     (diagram-fill diagram)
     ;; Dump PNG file
     (diagram-write-png diagram filename))))
