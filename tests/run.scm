(cond-expand
 (chicken-5
  (import (chicken port)
	  (chicken string)
	  (only (chicken io) write-byte)
	  (chicken memory)
	  test
	  stb-image-write
	  srfi-4))
 (else
  (use stb-image-write test srfi-4 ports extras)))

(define (write-all label w h c image)

  (define (saveto f p)
    (print "writing " f)
    (with-output-to-file f p))

  (define (jpg quality)
    (saveto (conc "out-" label "." c "q" quality ".jpg")
	    (lambda () (write-jpg w h c image quality))))
  (jpg 5) (jpg 30) (jpg 50) (jpg 80) (jpg 100)

  (saveto (conc "out-" label "." c ".png") (lambda () (write-png w h c image (* 3 w))))
  (saveto (conc "out-" label "." c ".bmp") (lambda () (write-bmp w h c image)))
  (saveto (conc "out-" label "." c ".tga") (lambda () (write-tga w h c image))))

(define image1
  (let ((x 255) (b 128) (_ 0))
    (u8vector b b b b b b b b b b
	      b _ _ _ _ _ _ _ _ b
	      b _ _ x _ x _ x _ b
	      b _ _ x _ x _ _ _ b
	      b _ _ x x x _ x _ b
	      b _ _ x _ x _ x _ b
	      b _ _ x _ x _ x _ b
	      b _ _ _ _ _ _ _ _ b
	      b _ _ _ _ _ _ _ _ b
	      b b b b b b b b b b)))
(write-all "c0" 10 10 1 image1)

;; ==================== creating bigger pictures ====================

(define (rgb r g b) (vector r g b))
(define (.r c) (vector-ref c 0))
(define (.g c) (vector-ref c 1))
(define (.b c) (vector-ref c 2))

(define (make-canvas pset data w h c) (vector pset data w h c))
(define (canvas-pset c) (vector-ref c 0))
(define (canvas-data c) (vector-ref c 1))
(define (canvas-w c) (vector-ref c 2))
(define (canvas-h c) (vector-ref c 3))
(define (canvas-c c) (vector-ref c 4))

(define (make-canvas-u8rgb w h)
  (make-canvas (lambda (data x y v)
		 (u8vector-set! data (+ (* 3 x) 0 (* 3 w y)) (.r v))
		 (u8vector-set! data (+ (* 3 x) 1 (* 3 w y)) (.g v))
		 (u8vector-set! data (+ (* 3 x) 2 (* 3 w y)) (.b v)))
	       (make-u8vector (* w h 3) 128)
	       w h 3))

(define (canvas-render c fi x0 y0 x1 y1)
  (do ((y y0 (+ y 1)))
      ((>= y y1))
    (do ((x x0 (+ x 1)))
	((>= x x1))
      ((canvas-pset c) (canvas-data c) x y (fi x y)))))

(define (canvas-save label c)
  (write-all label (canvas-w c) (canvas-h c) (canvas-c c) (canvas-data c)))

(define image
  (lambda (x y)
    (if (> (+ 50 x) y)
	(if (> 200 y)
	    (rgb 0 0 255)
	    (rgb (min 255 (floor (/ x 6))) 0 0))
	(rgb 0 255 0))))

(print "rendering c1 ...")
(define c1 (make-canvas-u8rgb 128 128))
(canvas-render c1 image 0 0 128 128)
(canvas-save "c1" c1)

(print "rendering c2 ...")
(define c2 (make-canvas-u8rgb 3840 2160))

(define scanline (make-canvas-u8rgb 3840 1))
(canvas-render scanline (lambda (x y) (rgb (modulo x 255) 0 0)) 0 0 3840 1)
(do ((y 0 (+ y 1)))
    ((>= y 2160))
  (move-memory! (canvas-data scanline)
		(canvas-data c2)
		#f 0 (* 3 (canvas-w c2) y)))
(canvas-render c2 image 100 100 800 800)
(canvas-save "c2" c2)


;; todo: test these too
;; write-hdr
;; ya ??
;; rgb
;; rgba
;; jpeg quality
;; png stride
