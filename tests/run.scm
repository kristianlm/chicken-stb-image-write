(cond-expand
 (chicken-5
  (import (chicken port)
	  (chicken string)
	  (only (chicken io) write-byte)
	  (only srfi-1 iota)
	  (chicken memory)
	  stb-image-write
	  srfi-4))
 (else
  (use stb-image-write (only srfi-1 iota) srfi-4 ports extras)))

(define (write-all* label image w h c flip?)

  (define fliplabel (if flip? '("flip") '()))
  (define colorlabel (cond ((= 1 c) "y")
			   ((= 2 c) "ya")
			   ((= 3 c) "rgb")
			   ((= 4 c) "rgba")))

  (define (labeler type . labels)
    (conc (string-intersperse
	   `("out" ,label ,type ,colorlabel
	     ,@(map conc (flatten labels))
	     ,@fliplabel) "-")
	  "." type))

  (define (saveto f p)
    (print* "writing " f "  \t")
    (time (with-output-to-file f p)))

  (define (jpg quality)
    (saveto (labeler "jpg" (conc "q" quality))
	    (lambda () (write-jpg image w h c quality: quality flip: flip?))))
  (jpg 5) (jpg 30) (jpg 50) (jpg 80) (jpg 100)

  (define (png compress filter)
    (saveto (labeler "png" (conc "z" compress) (if filter (conc "f" filter) '()))
	    (lambda () (write-png image w h c
			     filter: filter
			     compress: compress
			     flip: flip?))))
  (png 5 #f)  (png 8 #f)  (png 50 #f)
  (png 8  0)  (png 8  1)  (png 8 2) (png 8 3)

  (define (tga rle?)
    (saveto (labeler "tga" (if rle? "rle" '()))
	    (lambda () (write-tga image w h c flip: flip? rle: rle?))))
  (tga #t)
  (tga #f)

  (saveto (labeler "bmp") (lambda () (write-bmp image w h c flip: flip?))))

(define (write-all label image w h c)
  (write-all* label image w h c #f)
  (write-all* label image w h c #t))

(let* ((b 255) (r 128) (_ 0)
       (image (list->u8vector
	       (flatten
		(list b b b b b b b b b b b b b b b b
		      b _ _ _ _ _ _ _ _ _ _ _ _ _ _ b
		      b _ _ r r r _ r r r _ r r _ _ b
		      b _ _ r _ r _ r _ _ _ r _ r _ b
		      b _ _ r r r _ r r r _ r _ r _ b
		      b _ _ r r _ _ r _ _ _ r _ r _ b
		      b _ _ r _ r _ r r r _ r r _ _ b
		      b _ _ _ _ _ _ _ _ _ _ _ _ _ _ b
		      b b b b b b b b b b b b b b b b)))))
  (write-all "red" image 16 9 1))

(define (u8rgb-test w h)
  (let ((data (make-u8vector (* w h 3) 128))
	(scanline (make-u8vector (* 3 w) 0)))
    (for-each (lambda (i) (u8vector-set! scanline (* 3 i) (modulo i 256))) (iota w))
    (for-each (lambda (y) (move-memory! scanline data #f 0 (* 3 y w))) (iota h))
    (for-each
     (lambda (y)
       (for-each (lambda (x)
		   (u8vector-set! data (+ 1 (* 3 x) (* 3 w y)) #xff))
		 (iota 100)))
     (iota 100))
    (u8vector-set! data (+ 0 (* 3 10) (* 3 w 10)) #xff)
    (u8vector-set! data (+ 1 (* 3 10) (* 3 w 10)) #xff)
    (u8vector-set! data (+ 2 (* 3 10) (* 3 w 10)) #xff)
    data))

(let ((w 2048) (h 1024))
  (write-all "big" (u8rgb-test w h) w h 3))

(define (f32rgb-test w h)
  (let ((data (make-f32vector (* w h 3) 0.5))
	(scanline (make-f32vector (* 3 w) 0)))
    (for-each (lambda (i) (f32vector-set! scanline (* 3 i) (modulo i 256))) (iota w))
    (for-each (lambda (y) (move-memory! scanline data #f 0 (* 4 #|TODO|# 3 y w))) (iota h))
    (for-each
     (lambda (y)
       (for-each (lambda (x)
		   (f32vector-set! data (+ 1 (* 3 x) (* 3 w y)) #xff))
		 (iota 100)))
     (iota 100))
    (f32vector-set! data (+ 0 (* 3 10) (* 3 w 10)) #xff)
    (f32vector-set! data (+ 1 (* 3 10) (* 3 w 10)) #xff)
    (f32vector-set! data (+ 2 (* 3 10) (* 3 w 10)) #xff)
    data))


(let* ((b '(255 255)) (r '(255 128)) (_ '(0 0))
       (image (list->u8vector
	       (flatten
		(list b b b b b b b b b b b b b b b b
		      b _ _ _ _ _ _ _ _ _ _ _ _ _ _ b
		      b _ _ r r r _ r r r _ r r _ _ b
		      b _ _ r _ r _ r _ _ _ r _ r _ b
		      b _ _ r r r _ r r r _ r _ r _ b
		      b _ _ r r _ _ r _ _ _ r _ r _ b
		      b _ _ r _ r _ r r r _ r r _ _ b
		      b _ _ _ _ _ _ _ _ _ _ _ _ _ _ b
		      b b b b b b b b b b b b b b b b)))))
  (write-all "red" image 16 9 2))

(let* ((r '(255 0 0 255)) (b '(0 0 255 128)) (_ '(0 0 0 0))
       (image (list->u8vector
	       (flatten
		(list b b b b b b b b b b b b b b b b
		      b _ _ _ _ _ _ _ _ _ _ _ _ _ _ b
		      b _ _ r r r _ r r r _ r r _ _ b
		      b _ _ r _ r _ r _ _ _ r _ r _ b
		      b _ _ r r r _ r r r _ r _ r _ b
		      b _ _ r r _ _ r _ _ _ r _ r _ b
		      b _ _ r _ r _ r r r _ r r _ _ b
		      b _ _ _ _ _ _ _ _ _ _ _ _ _ _ b
		      b b b b b b b b b b b b b b b b)))))
  (write-all "red" image 16 9 4))
