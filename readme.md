  [stb_image_write]: https://github.com/nothings/stb/blob/master/stb_image_write.h


# CHICKEN bindings to [stb_image_write]

## Requirements

None, `stb_image_write.h` v1.09 is embedded in the egg.

## API

    [procedure] (write-png pixels width height components #!key flip compression stride filter)
    [procedure] (write-jpg pixels width height components #!key flip quality)
    [procedure] (write-tga pixels width height components #!key flip rle)
    [procedure] (write-bmp pixels width height components #!key flip)
	
Writes the u8vector `pixels` to `(current-output-port)` using the
image format of the procedure name. The pixel format depends on
`components`, which represents the number of bytes per pixel:

	1: y8 (gray)
	2: ya16 (gray + alpha)
	3: rgb24
	4: rgba32

Extra parameters are:

- `flip` specifies whether to flip the image vertically before outputting
- png `compression` sets the `zlib` compression quality. defaults to
  8, try higher values for more compression
- png `stride` is the distance in bytes from the first byte of a row
  of pixels to the first byte of the next row of pixels.
- png `filter` forces a particular [filter
  type](https://en.wikipedia.org/wiki/Portable_Network_Graphics#Filtering)
  for all scanlines. defaults to trying all and picking the most
  efficient (`#f`), other valid values are 0-4.
- jpg `quality` specifies lossy compression amount as a percentage (defaults to 80)
- tga `rle` enables or disable run-length-encoding compression. it is
  on by default, set to `#f` to disable rle compression

Note that on 32-bit systems, blobs cannot be larger than 16M. This
also limits image sizes as pixel data is stored in a blob.

## Example

```scheme
(use stb-image-write)

(define %pixels
  (let ((r '(255   0   0 255))
        (b '(  0   0 255 128))
        (_ '(  0   0   0   0)))
    (list->u8vector
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

(with-output-to-file "red.png"
  (lambda ()
    (write-png %pixels 16 9 4)))
```

## TODO

- test png stride
- see if it's possible to write data to port without copying memory

## Source

The source is located [here](https://github.com/kristianlm/chicken-stb-image-write).
