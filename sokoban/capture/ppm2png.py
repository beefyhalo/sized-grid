#!/usr/bin/env python3
"""PPM (P6) -> PNG, with nothing but the standard library.

The other half of sokoban-shot. That writes a PPM because a PPM is a header
and some bytes; this turns one into something a person or a model can look at.
Neither ImageMagick nor PIL is installed on the machine this was written for,
and sips does not read PPM, so the encoder is here: zlib and struct are enough
for a PNG.

    python3 ppm2png.py shot.ppm shot.png [downsample]

The downsample argument matters on a HiDPI display, where the framebuffer comes
back at twice the window: pass 2 to get back to window scale. It pools by
MAXIMUM rather than by nearest neighbour, and that is not a detail. A 1px
bright line on a dark field is exactly what this game draws -- the seam, the
goal rings -- and nearest neighbour drops half of them, which is how
sized-grid-23y3 came to diagnose a fake 'bottom-left quarter' pattern that was
an artefact of its own downsampling.
"""
import sys, zlib, struct

def read_ppm(path):
    d = open(path, 'rb').read()
    # header: P6 <ws> w <ws> h <ws> maxval <single ws>
    parts = []
    i = 2
    while len(parts) < 3:
        while i < len(d) and d[i:i+1].isspace(): i += 1
        if d[i:i+1] == b'#':
            while d[i:i+1] != b'\n': i += 1
            continue
        j = i
        while j < len(d) and not d[j:j+1].isspace(): j += 1
        parts.append(int(d[i:j])); i = j
    i += 1
    w, h, _ = parts
    return w, h, d[i:i+w*h*3]

def downsample(w, h, px, k):
    ow, oh = w // k, h // k
    out = bytearray(ow * oh * 3)
    for y in range(oh):
        for x in range(ow):
            best = None
            for dy in range(k):
                for dx in range(k):
                    o = ((y*k+dy)*w + (x*k+dx))*3
                    v = px[o:o+3]
                    # MAX pooling: keep the brightest sample in the block, so a
                    # 1px bright line on a dark field is not averaged away.
                    if best is None or sum(v) > sum(best): best = v
            out[(y*ow+x)*3:(y*ow+x)*3+3] = best
    return ow, oh, bytes(out)

def write_png(path, w, h, px):
    raw = b''.join(b'\x00' + px[y*w*3:(y+1)*w*3] for y in range(h))
    def chunk(t, d):
        c = t + d
        return struct.pack('>I', len(d)) + c + struct.pack('>I', zlib.crc32(c) & 0xffffffff)
    png = (b'\x89PNG\r\n\x1a\n'
           + chunk(b'IHDR', struct.pack('>IIBBBBB', w, h, 8, 2, 0, 0, 0))
           + chunk(b'IDAT', zlib.compress(raw, 9))
           + chunk(b'IEND', b''))
    open(path, 'wb').write(png)

if __name__ == '__main__':
    src, dst = sys.argv[1], sys.argv[2]
    k = int(sys.argv[3]) if len(sys.argv) > 3 else 1
    w, h, px = read_ppm(src)
    if k > 1:
        w, h, px = downsample(w, h, px, k)
    write_png(dst, w, h, px)
    print(f"{dst}: {w}x{h}")
