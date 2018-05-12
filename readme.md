  [stb_image_write]: https://github.com/nothings/stb/blob/master/stb_image_write.h


# CHICKEN bindings to [stb_image_write]

## Requirements

None, `stb_image_write.h` is embedded in the egg.

## API

    [procedure] (write-png width height components data #!key flip compression stride filter)
    [procedure] (write-jpg width height components data #!key flip quality)
    [procedure] (write-tga width height components data #!key flip rle)
    [procedure] (write-bmp width height components data #!key flip)
	
Writes the image data to `(current-output-port)` in the format of the
procedure name. `data` format depends on `components`, which
represents the number of bytes per pixel:

    1: 8 bits gray
	2: 8 bits gray + 8 bits alpha
	3: 8 bits of rgb
	4: 8 bits of rgb + 8 bits alpha

Extra parameters are:

- png `compression` sets the `zlib` compression quality. defaults to
  8, try higher values for more compression
- png `stride` is the distance in bytes from the first byte of a row
  of pixels to the first byte of the next row of pixels.
- png `filter` forces a particular [filter
  type](https://en.wikipedia.org/wiki/Portable_Network_Graphics#Filtering)
  for all scanlines. defaults to trying all and picking the most
  efficient (`#f`), other valid values are 0-4.
- tga `rle` enables or disable run-length-encoding compression. it is
  on by default, set to `#f` to disable rle compression

## TODO

- test png stride
- see if it's possible to write data to port without copying memory
