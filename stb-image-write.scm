(cond-expand
 (chicken-5
  (import (chicken foreign)
	  (only (chicken memory) move-memory!)
	  (only (chicken pretty-print) pp)
	  (only (chicken blob) blob?)
	  (chicken port)
	  (chicken process signal)
	  srfi-4)
  (import-for-syntax (only (chicken string) conc)))
 (else
  (use extras lolevel srfi-4 posix)
  (use-for-syntax (only extras conc))))

(foreign-declare "
#define STB_IMAGE_WRITE_IMPLEMENTATION
#define STBI_WRITE_NO_STDIO
#include \"stb_image_write.h\"
")

(define-external (port_write_func
		  ((c-pointer void) context)
		  ((c-pointer void) data)
		  (int size))
  void
  (let ((dest (make-u8vector size)))
    (move-memory! data dest size) ;; how to avoid copying?
    (write-u8vector dest)))

;; some writers (like tga ad bmp) send 1-byte callbacks. this makes
;; things very slow, so we buffer things in C first.
(foreign-declare "
#define C_WRITE_FUNC_BUFFER_SIZE 1024

void port_write_func(void*, void*, int);

typedef struct wf_buf {
 int pos;
 char buf[C_WRITE_FUNC_BUFFER_SIZE];
} wf_buf;

void cbuf_flush_to_port(wf_buf* buf) {
  if(buf->pos == 0) return;
  port_write_func(0, buf->buf, buf->pos);
  buf->pos = 0;
}

void cbuf_write_func(void* context, void* data, int size) {
 wf_buf *buf = (wf_buf*)context;
 if(buf->pos + size < C_WRITE_FUNC_BUFFER_SIZE) {
  memcpy(&buf->buf[buf->pos], data, size);
  buf->pos += size;
 } else {
  cbuf_flush_to_port(buf); // might fit now:
  if(buf->pos + size < C_WRITE_FUNC_BUFFER_SIZE) {
   memcpy(&buf->buf[buf->pos], data, size);
   buf->pos += size;
  } else { // still too big, straight to scheme
   port_write_func(0, data, size);
  }
 }
}
")

(define-syntax stbi-writer
  (er-macro-transformer
   (lambda (x r t)
     (let* ((foreign-safe-lambda* (r 'foreign-safe-lambda*))
	    (c_writer (cadr x))
	    (args-ta (caddr x)) ;; tad = type argname default
	    (args-a (map cadr args-ta))
	    (args_a (map (lambda (a) (conc ", " a)) args-a)))
       `(,foreign-safe-lambda*
	 int ((int w) (int h) (int c)
	      (scheme-pointer data)
	      ,@args-ta)
	 "wf_buf buf; buf.pos = 0;"
	 "int ret = " ,c_writer
	 "(cbuf_write_func, &buf, w, h, c, data" ,@args_a " );"
	 "cbuf_flush_to_port(&buf);"
	 "return(ret);")))))


(define (check result loc)
  (if (= 0 result)
      (error "error when writing image" loc)
      (void)))

(define (flip-set! flip?)
  ((foreign-lambda void "stbi_flip_vertically_on_write" int)
   (if flip? 1 0)))

(define (blobify data)
  (cond ((u8vector? data) (u8vector->blob/shared data))
	((string? data) data)
	((blob? data) data)
	(else (error "image data not string/blob/u8vector" data))))

(define (write-png data w h c #!key
		   flip (compression 8)
		   (stride 0) (filter #f))

  (flip-set! flip)

  ((foreign-lambda* void ((int filter)) "stbi_write_force_png_filter = filter;")
   (cond ((eq? filter #f) -1) ;; try all filters
	 ((and (<= filter 4) (>= filter 0)) filter)
	 (else (error "invalid filter range" filter))))

  ((foreign-lambda* void ((int compression))
		    "stbi_write_png_compression_level = compression;")
   compression)

  (check
   ((stbi-writer "stbi_write_png_to_func" ((int stride)))
    w h c (blobify data) stride)
   'write-png))

(define (write-jpg data w h c #!key flip (quality 80))
  (flip-set! flip)
  (check
   ((stbi-writer "stbi_write_jpg_to_func" ((int quality 80)))
    w h c (blobify data) quality)
   'write-jpg))

(define (write-bmp data w h c #!key flip)
  (flip-set! flip)
  (check ((stbi-writer "stbi_write_bmp_to_func" ()) w h c (blobify data))
	 'write-bmp))

(define (write-tga data w h c #!key flip (rle #t))
  (flip-set! flip)
  ((foreign-lambda* void ((int rle)) "stbi_write_tga_with_rle = rle;")
   (if rle 1 0))
  (check ((stbi-writer "stbi_write_tga_to_func" ()) w h c (blobify data))
	 'write-tga))

;; TODO: stm_image_write supports HDR, we could too. it's f32vector
