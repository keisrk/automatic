(use-modules (automatic proposition)
             (automatic diagram)
             (srfi srfi-42)
             (srfi srfi-64))

(display (dnf->brnf
          '(#( 1 #f #f #f #f)
            #(#f  1 #f #f #f)
            #(#f #f  1 #f #f)
            #(#f #f #f  1 #f)
            #(#f #f #f #f  1))
          ))(newline)

(display (dnf->brnf
          '(#( 0 #f #f #f #f)
            #(#f  0 #f #f #f)
            #(#f #f  0 #f #f)
            #(#f #f #f  0 #f)
            #(#f #f #f #f  0))
          ))(newline)

(define resources-dir (getenv "RESOURCES_DIR"))
(define arg-1 (list #:filename (string-append resources-dir "test-1") #:brnf (dnf->brnf
                                                                              ;;
                                                                              '(#( 1 #f #f #f #f)
                                                                                #(#f  1 #f #f #f)
                                                                                #(#f #f  1 #f #f)
                                                                                #(#f #f #f  1 #f)
                                                                                #(#f #f #f #f  1))) #:vars 5))

(define arg-2 (list #:filename (string-append resources-dir "test-2") #:brnf (dnf->brnf
                                                                              ;;'(((n 0) (n 1) (n 2) (n 3) (n 4)))
                                                                              '(#( 0  0  0  0  0))) #:vars 5))

(define arg-a (list #:filename (string-append resources-dir "test-a") #:brnf (dnf->brnf
                                                                              ;;'(((n 0) 1) (1 2)))
                                                                              '(#( 0 1 #f)
                                                                                #(#f 1  1))) #:vars 3))

(define arg-b (list #:filename (string-append resources-dir "test-b") #:brnf (dnf->brnf
                                                                              ;;'((0 1 (n 2)) ((n 1) 2) ((n 0) 1))
                                                                              '(#( 1 1  0)
                                                                                #(#f 0  1)
                                                                                #( 0 1 #f))) #:vars 3))

(draw-diagram arg-1)
(draw-diagram arg-2)
(draw-diagram arg-a)
(draw-diagram arg-b)
