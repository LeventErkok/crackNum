// Renders the CrackNum app icon into a macOS .iconset directory.
//
//   swift make-icon.swift <output.iconset>
//
// The Makefile's `icon` target runs this and then packs the PNGs into
// AppIcon.icns with `iconutil`. The artwork mirrors what crackNum shows: an
// IEEE-754 bit layout, coloured by field (sign / exponent / significand), sized
// here as half precision — 1 + 5 + 10 = 16 bits.

import AppKit
import Foundation

let outDir = CommandLine.arguments.count > 1 ? CommandLine.arguments[1] : "."

struct Field { let count: Int; let color: NSColor }
let fields = [
    Field(count:  1, color: NSColor(srgbRed: 0.93, green: 0.32, blue: 0.33, alpha: 1)), // sign
    Field(count:  5, color: NSColor(srgbRed: 0.31, green: 0.61, blue: 0.98, alpha: 1)), // exponent
    Field(count: 10, color: NSColor(srgbRed: 0.29, green: 0.80, blue: 0.55, alpha: 1)), // significand
]
let totalCells = fields.reduce(0) { $0 + $1.count }   // 16

/// Fill a rounded rect with a subtle top-lit vertical gradient of `base`.
func fillRounded(_ r: NSRect, radius: CGFloat, base: NSColor) {
    NSGraphicsContext.saveGraphicsState()
    NSBezierPath(roundedRect: r, xRadius: radius, yRadius: radius).addClip()
    let light = base.blended(withFraction: 0.30, of: .white) ?? base
    NSGradient(starting: light, ending: base)!.draw(in: r, angle: -90)
    NSGraphicsContext.restoreGraphicsState()
}

func render(_ px: Int) -> Data {
    let s = CGFloat(px)
    let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: px, pixelsHigh: px,
                              bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true,
                              isPlanar: false, colorSpaceName: .deviceRGB,
                              bytesPerRow: 0, bitsPerPixel: 0)!
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)!

    // Background squircle with a vertical navy gradient.
    let margin = s * 0.075
    let rect = NSRect(x: margin, y: margin, width: s - 2*margin, height: s - 2*margin)
    let radius = rect.width * 0.2237
    let bgPath = NSBezierPath(roundedRect: rect, xRadius: radius, yRadius: radius)
    NSGraphicsContext.saveGraphicsState()
    bgPath.addClip()
    NSGradient(starting: NSColor(srgbRed: 0.22, green: 0.30, blue: 0.50, alpha: 1),
               ending:   NSColor(srgbRed: 0.06, green: 0.09, blue: 0.18, alpha: 1))!
        .draw(in: rect, angle: -90)
    NSGraphicsContext.restoreGraphicsState()

    // Faint edge highlight for definition.
    NSColor.white.withAlphaComponent(0.10).setStroke()
    bgPath.lineWidth = max(1, s * 0.006)
    bgPath.stroke()

    // A row of bit-field cells, grouped and coloured by field.
    let pad = rect.width * 0.11
    let contentW = rect.width - 2*pad
    let cellGapR: CGFloat  = 0.18      // gap between cells, in cell widths
    let groupGapR: CGFloat = 0.62      // wider gap between fields
    let groupGaps = CGFloat(fields.count - 1)                 // 2
    let smallGaps = CGFloat(totalCells - 1) - groupGaps       // 13
    let denom = CGFloat(totalCells) + smallGaps*cellGapR + groupGaps*groupGapR
    let cellW = contentW / denom
    let cellGap = cellW * cellGapR
    let groupGap = cellW * groupGapR
    let cellH = rect.height * 0.30
    let y = rect.midY - cellH/2
    let cellR = cellW * 0.22

    var x = rect.minX + pad
    for (fi, field) in fields.enumerated() {
        for c in 0..<field.count {
            let cr = NSRect(x: x, y: y, width: cellW, height: cellH)
            fillRounded(cr, radius: cellR, base: field.color)
            let isLastOverall = (fi == fields.count - 1 && c == field.count - 1)
            if isLastOverall { continue }
            x += cellW + (c == field.count - 1 ? groupGap : cellGap)
        }
    }

    NSGraphicsContext.restoreGraphicsState()
    return rep.representation(using: .png, properties: [:])!
}

let sizes: [(String, Int)] = [
    ("icon_16x16.png",       16),
    ("icon_16x16@2x.png",    32),
    ("icon_32x32.png",       32),
    ("icon_32x32@2x.png",    64),
    ("icon_128x128.png",    128),
    ("icon_128x128@2x.png", 256),
    ("icon_256x256.png",    256),
    ("icon_256x256@2x.png", 512),
    ("icon_512x512.png",    512),
    ("icon_512x512@2x.png", 1024),
]

for (name, px) in sizes {
    let url = URL(fileURLWithPath: outDir).appendingPathComponent(name)
    try! render(px).write(to: url)
}
print("Wrote \(sizes.count) icon PNGs to \(outDir)")
