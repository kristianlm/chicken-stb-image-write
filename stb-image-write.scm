(cond-expand
 (chicken-5
  (import (chicken foreign)
	  (only (chicken memory) move-memory!)
	  (only (chicken pretty-print) pp)
	  (chicken process signal)
	  srfi-4))
 (else
  (use extras lolevel srfi-4 posix)))

(set-signal-handler! signal/segv #f)

(foreign-declare "
#define STB_IMAGE_WRITE_IMPLEMENTATION
#define STBI_WRITE_NO_STDIO
#include \"stb_image_write.h\"
")

(define-foreign-type stbi_write_func
  (function void
	    ((c-pointer void)
	     (c-pointer void)
	     int)))

(define-external (write_func
		  ((c-pointer void) context)
		  ((c-pointer void) data)
		  (int size))
  void
  (let ((dest (make-u8vector size)))
    (move-memory! data dest size) ;; how to avoid copying?
    (write-u8vector dest)))

(define write-png* (foreign-safe-lambda int "stbi_write_png_to_func" stbi_write_func (c-pointer void) int int int u8vector int #|stride|#))
(define write-jpg* (foreign-safe-lambda int "stbi_write_jpg_to_func" stbi_write_func (c-pointer void) int int int u8vector int #|quality|#))
(define write-bmp* (foreign-safe-lambda int "stbi_write_bmp_to_func" stbi_write_func (c-pointer void) int int int u8vector))
(define write-tga* (foreign-safe-lambda int "stbi_write_tga_to_func" stbi_write_func (c-pointer void) int int int u8vector))
(define write-hdr* (foreign-safe-lambda int "stbi_write_hdr_to_func" stbi_write_func (c-pointer void) int int int f32vector))

(define (check result loc)
  (if (= 0 result)
      (error "could not write image" loc)
      result))

(define (write-png width height components data #!optional (stride (* components width)))
  (check (write-png* (foreign-value "write_func" c-pointer) #f
		     width height components data stride)
	 'write-png))

(define (write-jpg width height components data quality)
  (check (write-jpg* (foreign-value "write_func" c-pointer) #f
		     width height components data quality)
	 'write-jpg))

(define (write-bmp width height components data)
  (check (write-bmp* (foreign-value "write_func" c-pointer) #f
		     width height components data)
	 'write-bmp))

(define (write-tga width height components data)
  (check (write-tga* (foreign-value "write_func" c-pointer) #f
		     width height components data)
	 'write-tga))

(define (write-hdr width height components data_f32)
  (check (write-hdr* (foreign-value "write_func" c-pointer) #f
		     width height components data_f32)
	 'write-hdr))

;; void stbi_flip_vertically_on_write(int flag); // flag is non-zero to flip data vertically
;; int stbi_write_tga_with_rle;             // defaults to true; set to 0 to disable RLE
;; int stbi_write_png_compression_level;    // defaults to 8; set to higher for more compression
;; int stbi_write_force_png_filter;         // defaults to -1; set to 0..5 to force a filter mode
