#!/usr/bin/env wish
# CrackNum GUI — Tcl/Tk front-end for the crackNum command-line tool.
# Works on Linux and macOS. Requires wish (Tk 8.5+) and crackNum on PATH.

# ---------------------------------------------------------------------------
# Tool discovery
# ---------------------------------------------------------------------------

proc locate {name} {
    if {[info exists ::env(PATH)]} { set raw $::env(PATH) } else { set raw "/usr/bin:/bin" }
    set path [split $raw :]
    foreach dir $path {
        set candidate [file join $dir $name]
        # 'file executable' is true for directories too (they carry the search
        # bit), so a directory named e.g. crackNum sitting on the PATH would
        # otherwise be picked up and then fail to execute. Insist on a file.
        if {[file isfile $candidate] && [file executable $candidate]} {
            return $candidate
        }
    }
    return ""
}

set CRACKNUM [locate crackNum]
set Z3       [locate z3]

# ---------------------------------------------------------------------------
# Format table
# ---------------------------------------------------------------------------
# Each entry: {id label flag_kind flag_arg}
#   flag_kind = fixed | customFloat | word | customWord | int | customInt
#   flag_arg  = the flag suffix for "fixed", or bit-count for "word"/"int"

set FORMAT_SECTIONS {
    {"Float" {
        {ffp4  "FP4 (E2M1)"  fixed    fp4}
        {fe4m3 "FP8 (E4M3)"  fixed    e4m3}
        {fe5m2 "FP8 (E5M2)"  fixed    e5m2}
        {fhp   "Half"        fixed    hp}
        {fbp   "Brain"       fixed    bp}
        {ftf32 "TF32"        fixed    tf32}
        {fsp   "Single"      fixed    sp}
        {fdp   "Double"      fixed    dp}
        {fqp   "Quad"        fixed    qp}
        {fcs   "Custom"      customFloat {}}
    }}
    {"Word (Unsigned)" {
        {w8   "8-bit"   word    8}
        {w16  "16-bit"  word   16}
        {w32  "32-bit"  word   32}
        {w64  "64-bit"  word   64}
        {wcs  "Custom"  customWord {}}
    }}
    {"Integer (Signed)" {
        {i8   "8-bit"   int    8}
        {i16  "16-bit"  int   16}
        {i32  "32-bit"  int   32}
        {i64  "64-bit"  int   64}
        {ics  "Custom"  customInt {}}
    }}
}

set ROUNDING_MODES {RNE RNA RTP RTN RTZ}
array set ROUNDING_LABELS {
    RNE "RNE (Nearest, ties to even)"
    RNA "RNA (Nearest, ties to away)"
    RTP "RTP (Toward +∞)"
    RTN "RTN (Toward -∞)"
    RTZ "RTZ (Toward 0)"
}

# ---------------------------------------------------------------------------
# State
# ---------------------------------------------------------------------------
set state(selection) ""   ;# selected format id
set state(value)     ""
set state(rounding)  "RNE"
set state(bitWidth)  64
set state(expWidth)  11
set state(fontSize)  11

set WELCOME {Enter a value above, then pick a format on the left to crack it.

You can:
  - ENCODE: from a mathematical value to its internal representation
  - DECODE: from an internal representation to its mathematical value

Encoding:
  - Enter a decimal value (2.5, -4.1e5) or hex float (0x2.4p3).
  - You can pass NaN, Inf, -0, -Inf for special values.
  - For floats, pick a rounding mode.
  - Input must NOT start with 0x, 0b, or N'h (else we decode instead).

Decoding:
  - Use hex (0x), binary (0b), or Verilog (N'h) notation.
  - You may use _, - or space as separators for readability.
  - Verilog input longer than the format is decoded as SIMD lanes.}

# ---------------------------------------------------------------------------
# Build the precision flag from the selected format
# ---------------------------------------------------------------------------
proc precisionFlag {} {
    global state FORMAT_SECTIONS

    set sel $state(selection)
    if {$sel eq ""} { return "" }

    foreach section $FORMAT_SECTIONS {
        foreach fmt [lindex $section 1] {
            lassign $fmt id label kind arg
            if {$id ne $sel} continue

            switch $kind {
                fixed       { return "-f$arg" }
                word        { return "-w$arg" }
                int         { return "-i$arg" }
                customWord  { return "-w$state(bitWidth)" }
                customInt   { return "-i$state(bitWidth)" }
                customFloat {
                    # Only check that the widths describe a well-formed layout; crackNum
                    # itself owns the remaining limits (and reports solver restrictions
                    # readably).
                    set bw $state(bitWidth)
                    set ew $state(expWidth)
                    set sig [expr {$bw - $ew - 1}]
                    if {$ew < 1 || $sig < 0} {
                        return [list invalid \
"Invalid custom FP format:
  Total width: $bw
    Sign       :    1
    Exponent   : [format %4d $ew]
    Significand: [format %4d $sig] (Total = Sign + Exponent + Significand)

Exponent must be at least 1 bit, and the total width must leave room for it and the sign."]
                    }
                    return "-f${ew}+[expr {$bw - $ew}]"
                }
            }
        }
    }
    return ""
}

# ---------------------------------------------------------------------------
# Run crackNum and return output text
# ---------------------------------------------------------------------------
proc runCrackNum {} {
    global state CRACKNUM Z3

    if {$CRACKNUM eq ""} {
        return "crackNum: Cannot locate the 'crackNum' binary on your PATH.\n\nMake sure it is installed and reachable (e.g. `which crackNum` works in your terminal)."
    }
    if {$Z3 eq ""} {
        return "crackNum: Cannot locate the 'z3' binary on your PATH.\n\nMake sure it is installed and reachable (e.g. `which z3` works in your terminal)."
    }

    set flagResult [precisionFlag]
    if {$flagResult eq ""} { return "" }

    if {[lindex $flagResult 0] eq "invalid"} {
        return [lindex $flagResult 1]
    }

    set flag $flagResult
    set rm   "-r$state(rounding)"

    # NB: do NOT use [expr] to default this. expr parses its operands as numbers,
    # so "0xdeadbeef" would arrive as 3735928559 and crackNum would encode the
    # decimal instead of decoding the bit-pattern.
    set val $state(value)
    if {$val eq ""} { set val 0 }

    # Pass SBV_Z3 so crackNum finds z3 even when PATH is minimal.
    if {[info exists ::env(SBV_Z3)]} { set savedZ3 $::env(SBV_Z3) } else { set savedZ3 "" }
    set ::env(SBV_Z3) $Z3

    # We never pass -l: crackNum infers the lane count from Verilog (N'h) input,
    # and everything else is a single lane.
    set cmd [list $CRACKNUM $flag $rm -- $val]

    # 2>@1 folds stderr into the captured result, so errors show up in the pane.
    set rc [catch {exec {*}$cmd 2>@1} output]

    if {$savedZ3 eq ""} { unset -nocomplain ::env(SBV_Z3) } \
    else                { set ::env(SBV_Z3) $savedZ3 }

    if {$rc && ![string match "*ENCODED*" $output] && ![string match "*DECODED*" $output]} {
        append output "\n\n** Call to crackNum failed! Make sure the value makes sense for the chosen format."
        append output "\n**"
        append output "\n**   Run: $cmd"
        append output "\n**"
        append output "\n**   Value : $val"
    }
    return $output
}

# ---------------------------------------------------------------------------
# Show output in the text widget
# ---------------------------------------------------------------------------
proc showOutput {text} {
    .output configure -state normal
    .output delete 1.0 end
    .output insert end $text
    .output configure -state disabled
}

proc crack {} {
    global state FORMAT_SECTIONS

    set sel $state(selection)
    if {$sel eq ""} return

    set out [runCrackNum]

    # Determine label for header
    set label ""
    foreach section $FORMAT_SECTIONS {
        foreach fmt [lindex $section 1] {
            if {[lindex $fmt 0] eq $sel} { set label [lindex $fmt 1] }
        }
    }

    if {[string match "*ENCODED*" $out]}      { set kind "Encoding in format" } \
    elseif {[string match "*DECODED*" $out]}  { set kind "Decoded using format" } \
    else                                      { set kind "Format" }

    showOutput "\[$kind: $label\]\n\n$out"
}

# ---------------------------------------------------------------------------
# Font selection
# ---------------------------------------------------------------------------
# Pick a monospaced family with sane vertical metrics. Plain "Courier" is an
# X11 alias that commonly resolves to Nimbus Mono PS, whose ascent/descent split
# is badly lopsided (9/6 at size 11). Since an entry centers on the linespace,
# that leaves the glyphs hugging the top of the box with a fat gap underneath.
proc pickMonoFamily {} {
    set installed {}
    foreach f [font families] { lappend installed [string tolower $f] }
    foreach want {"DejaVu Sans Mono" "Menlo" "Liberation Mono" "Consolas" "Courier New"} {
        if {[lsearch -exact $installed [string tolower $want]] >= 0} { return $want }
    }
    return "Courier"
}

set MONO [pickMonoFamily]

# ---------------------------------------------------------------------------
# Font size helpers
# ---------------------------------------------------------------------------
proc applyFontSize {} {
    global state MONO
    .output configure -font [list $MONO $state(fontSize)]
}

proc zoomIn  {} { incr ::state(fontSize); applyFontSize }
proc zoomOut {} {
    if {$::state(fontSize) > 6} { incr ::state(fontSize) -1; applyFontSize }
}

# ---------------------------------------------------------------------------
# Build UI
# ---------------------------------------------------------------------------

wm title . "CrackNum"
wm minsize . 1200 700
wm geometry . 1200x700

image create photo appIcon -data {
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
rkJggg==
}
wm iconphoto . appIcon

# ---- Menu bar ------------------------------------------------------------
menu .mb -tearoff 0
. configure -menu .mb

menu .mb.file -tearoff 0
.mb add cascade -label "File" -menu .mb.file -underline 0
.mb.file add command -label "Close" -accelerator "Ctrl+W" -command { destroy . }
bind . <Control-w> { destroy . }
bind . <Control-q> { destroy . }

menu .mb.edit -tearoff 0
.mb add cascade -label "Edit" -menu .mb.edit -underline 0
.mb.edit add command -label "Cut"        -accelerator "Ctrl+X" -command { event generate [focus] <<Cut>> }
.mb.edit add command -label "Copy"       -accelerator "Ctrl+C" -command { event generate [focus] <<Copy>> }
.mb.edit add command -label "Paste"      -accelerator "Ctrl+V" -command { event generate [focus] <<Paste>> }
.mb.edit add separator
.mb.edit add command -label "Select All" -accelerator "Ctrl+A" -command { event generate [focus] <<SelectAll>> }

# ---- Top bar: zoom, help, value entry ------------------------------------
frame .top
pack .top -fill x -padx 8 -pady 6

foreach {fr txt fnt cmd} {
    .top.zf1 "A" {TkDefaultFont 8}  zoomOut
    .top.zf2 "A" {TkDefaultFont 14} zoomIn
} {
    frame $fr -width 28 -height 28
    pack propagate $fr 0
    button $fr.b -text $txt -font $fnt -command $cmd
    pack $fr.b -fill both -expand yes
    pack $fr -side left -padx 2
}
button .top.help -text "?" -command { showOutput $::WELCOME }
pack .top.help -side left -padx 2

entry .top.val -textvariable state(value) -font [list $MONO 11] -width 28
pack .top.val -side right -padx {0 4}
bind .top.val <Return> crack

label .top.lbl -text "Value:"
pack .top.lbl -side right -padx {4 2}

# ---- Main pane: sidebar + output ----------------------------------------
frame .main
pack .main -fill both -expand yes -padx 8 -pady {0 8}

# Sidebar
frame .main.side -width 240
pack .main.side -side left -fill y -padx {0 6}
pack propagate .main.side 0

# Format list
ttk::style configure Treeview       -rowheight 22
ttk::style configure Treeview.Item  -padding {4 0}

ttk::treeview .main.side.lb -selectmode browse -show tree -height 18
pack .main.side.lb -fill both -expand yes

.main.side.lb tag configure hdr  -font {TkDefaultFont 9 bold}
.main.side.lb tag configure item -font {TkDefaultFont 9}

# Populate treeview; build item-id <-> format-id mappings
array set ITEM_FMT {}   ;# treeview item id -> format id
array set FMT_ITEM {}   ;# format id -> treeview item id

foreach section $FORMAT_SECTIONS {
    set title [lindex $section 0]
    set sid [.main.side.lb insert {} end -text $title -open yes -tags hdr]
    foreach fmt [lindex $section 1] {
        set fid  [lindex $fmt 0]
        set iid  [.main.side.lb insert $sid end -text [lindex $fmt 1] -tags item]
        set ITEM_FMT($iid) $fid
        set FMT_ITEM($fid) $iid
    }
}

bind .main.side.lb <<TreeviewSelect>> {
    set sel [.main.side.lb selection]
    if {$sel ne "" && [info exists ITEM_FMT($sel)]} {
        set state(selection) $ITEM_FMT($sel)
        crack
    } else {
        .main.side.lb selection remove $sel
    }
}

# Rounding
frame .main.side.rm
pack .main.side.rm -fill x -pady {6 0}
label .main.side.rm.lbl -text "Rounding mode:" -anchor w
pack .main.side.rm.lbl -fill x
ttk::combobox .main.side.rm.cb -state readonly -width 28
pack .main.side.rm.cb -fill x

foreach rm {RNE RNA RTP RTN RTZ} { lappend rm_labels $::ROUNDING_LABELS($rm) }
.main.side.rm.cb configure -values $rm_labels
# Show the full label in the combo but store the code in state(rounding)
proc rmLabel2Code {label} {
    foreach rm {RNE RNA RTP RTN RTZ} {
        if {$::ROUNDING_LABELS($rm) eq $label} { return $rm }
    }
    return RNE
}
proc rmCode2Label {code} { return $::ROUNDING_LABELS($code) }
.main.side.rm.cb set [rmCode2Label $state(rounding)]
bind .main.side.rm.cb <<ComboboxSelected>> {
    set state(rounding) [rmLabel2Code [.main.side.rm.cb get]]
    crack
}

# Custom parameters
labelframe .main.side.custom -text "Custom parameters" -padx 4 -pady 4
pack .main.side.custom -fill x -pady {8 0}

frame .main.side.custom.bw
pack .main.side.custom.bw -fill x -pady 2
label .main.side.custom.bw.l -text "Total width:"
pack  .main.side.custom.bw.l -side left
entry .main.side.custom.bw.e -textvariable state(bitWidth) -width 6 -justify right \
    -font [list $MONO 11]
pack  .main.side.custom.bw.e -side right
bind  .main.side.custom.bw.e <Return> crack

frame .main.side.custom.ew
pack .main.side.custom.ew -fill x -pady 2
label .main.side.custom.ew.l -text "Exponent width:"
pack  .main.side.custom.ew.l -side left
entry .main.side.custom.ew.e -textvariable state(expWidth) -width 6 -justify right \
    -font [list $MONO 11]
pack  .main.side.custom.ew.e -side right
bind  .main.side.custom.ew.e <Return> crack

label .main.side.custom.note \
    -text "(exponent width applies to custom floats)" \
    -font {TkDefaultFont 8} -foreground gray -wraplength 200 -justify left
pack .main.side.custom.note -fill x -pady {4 0}

# Output pane
frame .main.out
pack .main.out -side left -fill both -expand yes

text .output -state disabled -wrap none \
    -font [list $MONO $state(fontSize)] \
    -padx 8 -pady 8 \
    -xscrollcommand {.main.out.sx set} \
    -yscrollcommand {.main.out.sy set}
scrollbar .main.out.sy -orient vertical   -command {.output yview}
scrollbar .main.out.sx -orient horizontal -command {.output xview}

grid .output       .main.out.sy -in .main.out -sticky nsew
grid .main.out.sx               -in .main.out -sticky ew
grid columnconfigure .main.out 0 -weight 1
grid rowconfigure    .main.out 0 -weight 1

# ---------------------------------------------------------------------------
# Parse crackNum-style command-line args (forwarded by `crackNum --gui ...`)
# ---------------------------------------------------------------------------
proc parseArgs {argv} {
    global state FORMAT_SECTIONS FMT_ITEM

    set values {}
    set i 0
    while {$i < [llength $argv]} {
        set a [lindex $argv $i]
        if {$a eq "--"} {
            lappend values {*}[lrange $argv [expr {$i+1}] end]
            break
        }
        if {[string match "-f*" $a]} {
            set v [string tolower [string range $a 2 end]]
            switch $v {
                sp   { set state(selection) fsp }
                dp   { set state(selection) fdp }
                qp   { set state(selection) fqp }
                hp   { set state(selection) fhp }
                bp   { set state(selection) fbp }
                tf32 { set state(selection) ftf32 }
                e4m3 { set state(selection) fe4m3 }
                e5m2 { set state(selection) fe5m2 }
                fp4  { set state(selection) ffp4 }
                default {
                    if {[regexp {^(\d+)\+(\d+)$} $v _ e s]} {
                        set state(selection) fcs
                        set state(expWidth)  $e
                        set state(bitWidth)  [expr {$e + $s}]
                    }
                }
            }
        } elseif {[string match "-w*" $a]} {
            set v [string range $a 2 end]
            if {$v in {8 16 32 64}} { set state(selection) w$v } \
            elseif {[string is integer -strict $v] && $v > 0} {
                set state(selection) wcs
                set state(bitWidth) $v
            }
        } elseif {[string match "-i*" $a]} {
            set v [string range $a 2 end]
            if {$v in {8 16 32 64}} { set state(selection) i$v } \
            elseif {[string is integer -strict $v] && $v > 0} {
                set state(selection) ics
                set state(bitWidth) $v
            }
        } elseif {[string match "-r*" $a]} {
            set rm [string toupper [string range $a 2 end]]
            if {$rm in {RNE RNA RTP RTN RTZ}} { set state(rounding) $rm }
        } elseif {![string match "-*" $a]} {
            lappend values $a
        }
        incr i
    }
    if {[llength $values]} { set state(value) [join $values " "] }

    # Sync treeview selection highlight
    if {$state(selection) ne "" && [info exists FMT_ITEM($state(selection))]} {
        set iid $FMT_ITEM($state(selection))
        .main.side.lb selection set $iid
        .main.side.lb see $iid
    }

    # Sync rounding combo label. Must happen even when no format was given:
    # `crackNum --gui -rRTZ` sets state(rounding) but leaves no selection.
    .main.side.rm.cb set [rmCode2Label $state(rounding)]
}

# ---------------------------------------------------------------------------
# Start
# ---------------------------------------------------------------------------
showOutput $WELCOME
parseArgs $argv
if {$state(selection) ne ""} { crack }
focus .top.val
.top.val icursor end
