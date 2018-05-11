(module stb-image-write (write-png write-jpg write-bmp write-tga)
(cond-expand
 (chicken-5 (import scheme (chicken base)))
 (else      (use scheme chicken)))
(include "stb-image-write.scm"))
