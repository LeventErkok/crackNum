#!/usr/bin/env python3

"""Pack the CrackNum app icon into CrackNum.ico for the Windows GUI.

The artwork is not defined here. It comes from GUI/swiftGUI/make-icon.swift, the
same generator that produces the macOS AppIcon.icns, so the two platforms cannot
drift apart. This script only re-containers its PNGs as a Windows .ico.

Usage, from the repository root (needs a Swift toolchain for the render step):

    mkdir -p /tmp/crackNum.iconset
    swift GUI/swiftGUI/make-icon.swift /tmp/crackNum.iconset
    python3 GUI/winGUI/make-ico.py /tmp/crackNum.iconset GUI/winGUI/CrackNum.ico

The result is committed, as AppIcon.icns is, so that neither a Swift toolchain
nor Python is needed to build the GUI.
"""

import os
import struct
import sys

# Windows wants 16/32/48/64/128/256. The macOS generator emits power-of-two sizes
# only, so there is no 48: Windows downscales the 64 for the few places that ask
# for it, which is invisible on artwork this flat.
WANTED = [
    (16,  'icon_16x16.png'),
    (32,  'icon_32x32.png'),
    (64,  'icon_32x32@2x.png'),
    (128, 'icon_128x128.png'),
    (256, 'icon_256x256.png'),
]

PNG_MAGIC = b'\x89PNG\r\n\x1a\n'


def load(srcdir):
    images = []
    for px, name in WANTED:
        with open(os.path.join(srcdir, name), 'rb') as handle:
            data = handle.read()

        # Check the PNG's real dimensions rather than trusting the file name: the
        # macOS naming scheme means icon_32x32@2x.png is a 64px image, and getting
        # that mapping wrong would produce an .ico that renders at the wrong size
        # without ever failing to load.
        if data[:8] != PNG_MAGIC:
            raise SystemExit('%s is not a PNG' % name)

        width, height = struct.unpack('>II', data[16:24])
        if (width, height) != (px, px):
            raise SystemExit('%s is %dx%d, expected %dx%d' % (name, width, height, px, px))

        images.append((px, data))

    return images


def pack(images):
    # ICONDIR: reserved, type (1 = icon), image count.
    out = struct.pack('<HHH', 0, 1, len(images))

    offset = 6 + 16 * len(images)
    entries = b''
    payload = b''

    for px, data in images:
        # The width and height fields are a single byte each, so 256 is encoded
        # as 0. Entries store the PNG verbatim, which Windows has accepted since
        # Vista and which keeps the file a fraction of the BMP equivalent's size.
        dim = 0 if px == 256 else px
        entries += struct.pack('<BBBBHHII', dim, dim, 0, 0, 1, 32, len(data), offset)
        payload += data
        offset += len(data)

    return out + entries + payload


def main():
    if len(sys.argv) != 3:
        raise SystemExit('usage: make-ico.py <iconset-dir> <output.ico>')

    srcdir, out = sys.argv[1], sys.argv[2]
    blob = pack(load(srcdir))

    with open(out, 'wb') as handle:
        handle.write(blob)

    print('wrote %s: %d sizes, %d bytes' % (out, len(WANTED), len(blob)))


if __name__ == '__main__':
    main()
