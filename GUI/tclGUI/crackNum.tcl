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
#
# The floats are grouped by provenance rather than by width: first the formats that
# exist because of machine learning (the narrow FP4/FP8 ones, plus bfloat16 and
# TF32), then the IEEE-754 ones. Order here is the order the sidebar shows. The ids
# are what parseArgs maps -f/-w/-i onto, so they stay put even when a format moves
# from one group to another.

set FORMAT_SECTIONS {
    {"AI formats" {
        {ffp4     "FP4 (E2M1)"  fixed    fp4}
        {ffp4e0m3 "FP4 (E0M3)"  fixed    fp4e0m3}
        {fe4m3    "FP8 (E4M3)"  fixed    e4m3}
        {fe5m2    "FP8 (E5M2)"  fixed    e5m2}
        {fe8m0    "FP8 (E8M0)"  fixed    e8m0}
        {fbp      "Brain"       fixed    bp}
        {ftf32    "TF32"        fixed    tf32}
    }}
    {"IEEE-754" {
        {fhp      "Half"        fixed    hp}
        {fsp      "Single"      fixed    sp}
        {fdp      "Double"      fixed    dp}
        {fqp      "Quad"        fixed    qp}
        {fcs      "Custom"      customFloat {}}
    }}
    {"Integer (Signed)" {
        {i8   "8-bit"   int    8}
        {i16  "16-bit"  int   16}
        {i32  "32-bit"  int   32}
        {i64  "64-bit"  int   64}
        {ics  "Custom"  customInt {}}
    }}
    {"Word (Unsigned)" {
        {w8   "8-bit"   word    8}
        {w16  "16-bit"  word   16}
        {w32  "32-bit"  word   32}
        {w64  "64-bit"  word   64}
        {wcs  "Custom"  customWord {}}
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
  - Verilog N'h: N is the total width, split into N/format-size lanes.}

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
                    #
                    # The width entries are free-form text with no -validatecommand, so
                    # normalize them the way the Swift and Windows GUIs do: anything that
                    # is not a plain decimal integer becomes 0 and falls into the message
                    # below. This has to happen before any arithmetic -- expr throws on a
                    # non-numeric operand ("can't use non-numeric string as operand"), and
                    # so does the [format %4d] in that message, and an error raised inside
                    # a widget binding escapes as a Tk error dialog instead.
                    #
                    # Tested with a regexp rather than [string is integer], which is true
                    # for "0x20" and would let expr silently read it as 32. Swift's
                    # Int("0x20") and C#'s int.TryParse both reject it, and matching them
                    # is what keeps the three GUIs agreeing.
                    set bw $state(bitWidth)
                    set ew $state(expWidth)
                    if {![regexp {^-?[0-9]+$} [string trim $bw]]} { set bw 0 }
                    if {![regexp {^-?[0-9]+$} [string trim $ew]]} { set ew 0 }
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

    # An empty box is not a value. This used to default to 0, which cracked a number
    # the user had never typed and presented the result exactly like a real one. Say
    # what is missing instead of inventing input. (Note there is deliberately no
    # [expr] here: expr parses its operands as numbers, so "0xdeadbeef" would arrive
    # as 3735928559 and crackNum would encode the decimal instead of decoding it.)
    set val $state(value)
    if {[string trim $val] eq ""} { return "Enter a value above to crack it." }

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

# Tk has no auto-hiding scrollbar, so do it by hand: unpack it while the whole list
# fits, and pack it back when it does not. -before keeps it to the right of the
# treeview when it returns, matching how it was packed originally.
#
# NB. Safe against the oscillation that auto-hiding is prone to, because hiding a
# *vertical* scrollbar only makes the treeview wider, which cannot change the vertical
# fractions that decide whether it should be shown. That reasoning does not carry over
# to a horizontal/vertical pair, where each one's visibility feeds the other's.
proc autoScroll {sb tv first last} {
    if {$first <= 0.0 && $last >= 1.0} {
        if {[winfo manager $sb] ne ""} { pack forget $sb }
    } elseif {[winfo manager $sb] eq ""} {
        pack $sb -side right -fill y -before $tv
    }
    $sb set $first $last
}

# The format list is taller than the sidebar at the default window size, so it needs
# a scrollbar: without one the rows past the bottom are not merely off-screen, they
# are unreachable. Treeview and scrollbar live in their own frame so the widgets
# packed below (rounding, custom parameters) are unaffected by the side-by-side
# packing used here.
frame .main.side.fmts
pack .main.side.fmts -fill both -expand yes

ttk::treeview .main.side.fmts.lb -selectmode browse -show tree -height 18 \
    -yscrollcommand {autoScroll .main.side.fmts.sy .main.side.fmts.lb}
ttk::scrollbar .main.side.fmts.sy -orient vertical -command {.main.side.fmts.lb yview}
pack .main.side.fmts.sy -side right -fill y
pack .main.side.fmts.lb -side left -fill both -expand yes

.main.side.fmts.lb tag configure hdr  -font {TkDefaultFont 9 bold}
.main.side.fmts.lb tag configure item -font {TkDefaultFont 9}

# Populate treeview; build item-id <-> format-id mappings
array set ITEM_FMT {}   ;# treeview item id -> format id
array set FMT_ITEM {}   ;# format id -> treeview item id

foreach section $FORMAT_SECTIONS {
    set title [lindex $section 0]
    set sid [.main.side.fmts.lb insert {} end -text $title -open yes -tags hdr]
    foreach fmt [lindex $section 1] {
        set fid  [lindex $fmt 0]
        set iid  [.main.side.fmts.lb insert $sid end -text [lindex $fmt 1] -tags item]
        set ITEM_FMT($iid) $fid
        set FMT_ITEM($fid) $iid
    }
}

bind .main.side.fmts.lb <<TreeviewSelect>> {
    set sel [.main.side.fmts.lb selection]
    if {$sel ne "" && [info exists ITEM_FMT($sel)]} {
        set state(selection) $ITEM_FMT($sel)
        syncCustomBox
        crack
    } else {
        .main.side.fmts.lb selection remove $sel
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

# Custom parameters. The heading is a separate label above the box rather than the
# labelframe's own -text: that keeps the framed/shaded container while letting the
# heading line up flush left with "Rounding mode:" above it, instead of being
# indented past it by the frame's title inset.
# Text is a placeholder: syncCustomBox retitles this to name whatever is selected.
label .main.side.customLbl -text "Custom format:" -anchor w
pack  .main.side.customLbl -fill x -pady {8 2}

labelframe .main.side.custom -padx 4 -pady 4
pack .main.side.custom -fill x

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

# The box above is shared by all three "Custom" entries -- IEEE-754 float, signed
# integer, and unsigned word -- but only the float has an exponent, and with a fixed
# format selected nothing in the box does anything at all. So the heading and which
# rows stay live both follow the selection, rather than naming one of the three and
# hoping. Call this on every selection change.
proc syncCustomBox {} {
    global state FORMAT_SECTIONS

    set kind ""
    foreach section $FORMAT_SECTIONS {
        foreach fmt [lindex $section 1] {
            if {[lindex $fmt 0] eq $state(selection)} { set kind [lindex $fmt 2] }
        }
    }

    switch $kind {
        customFloat { set heading "Custom IEEE-754 float:" ; set bw normal   ; set ew normal   }
        customInt   { set heading "Custom signed integer:" ; set bw normal   ; set ew disabled }
        customWord  { set heading "Custom unsigned word:"  ; set bw normal   ; set ew disabled }
        default     { set heading "Custom format:"         ; set bw disabled ; set ew disabled }
    }

    .main.side.customLbl   configure -text  $heading
    .main.side.custom.bw.l configure -state $bw
    .main.side.custom.bw.e configure -state $bw
    .main.side.custom.ew.l configure -state $ew
    .main.side.custom.ew.e configure -state $ew
}

# Output pane
frame .main.out
pack .main.out -side left -fill both -expand yes

# Auto-hiding for the output pane's pair. Unlike the format list's lone vertical bar,
# these two feed each other: dropping the horizontal bar makes the text taller, which
# can change the vertical fractions, and dropping the vertical one makes it wider,
# which can change the horizontal ones. A naive toggle can therefore oscillate when
# the content sits right at the boundary. Three guards:
#
#   * track visibility ourselves rather than re-deriving it from the widget, so the
#     decision does not depend on geometry that is still settling;
#   * act only on an actual change of visibility;
#   * defer the change to an idle callback, so a burst of scrollcommand calls during
#     one geometry pass collapses into a single decision.
#
# 'grid remove' (rather than 'grid forget') keeps the row/column options, so restoring
# it is a bare 'grid'.
array set SB_VIS     {}   ;# scrollbar -> 1 when currently gridded
array set SB_PENDING {}   ;# scrollbar -> 1 when an idle update is already queued

proc autoScrollGrid {sb first last} {
    global SB_VIS SB_PENDING
    $sb set $first $last
    if {![info exists SB_VIS($sb)]} { set SB_VIS($sb) 1 }
    set want [expr {($first <= 0.0 && $last >= 1.0) ? 0 : 1}]
    if {$want == $SB_VIS($sb) || [info exists SB_PENDING($sb)]} return
    set SB_PENDING($sb) 1
    after idle [list applyScrollVis $sb $want]
}

proc applyScrollVis {sb want} {
    global SB_VIS SB_PENDING
    unset -nocomplain SB_PENDING($sb)
    if {$want == $SB_VIS($sb)} return
    if {$want} { grid $sb } else { grid remove $sb }
    set SB_VIS($sb) $want
}

text .output -state disabled -wrap none \
    -font [list $MONO $state(fontSize)] \
    -padx 8 -pady 8 \
    -xscrollcommand {autoScrollGrid .main.out.sx} \
    -yscrollcommand {autoScrollGrid .main.out.sy}
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
                sp      { set state(selection) fsp }
                dp      { set state(selection) fdp }
                qp      { set state(selection) fqp }
                hp      { set state(selection) fhp }
                bp      { set state(selection) fbp }
                tf32    { set state(selection) ftf32 }
                e4m3    { set state(selection) fe4m3 }
                e5m2    { set state(selection) fe5m2 }
                fp4     { set state(selection) ffp4 }
                fp4e0m3 { set state(selection) ffp4e0m3 }
                e8m0    { set state(selection) fe8m0 }
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
        .main.side.fmts.lb selection set $iid
        .main.side.fmts.lb see $iid
    }

    # Sync rounding combo label. Must happen even when no format was given:
    # `crackNum --gui -rRTZ` sets state(rounding) but leaves no selection.
    .main.side.rm.cb set [rmCode2Label $state(rounding)]
}

# ---------------------------------------------------------------------------
# Footer: the version, and where to report what it gets wrong
# ---------------------------------------------------------------------------

set ISSUES_URL "https://github.com/LeventErkok/crackNum/issues"

# Ask the binary its version rather than carrying a copy here, which would drift
# the first time a release bumped the cabal file and not this file. Empty if
# crackNum is missing or says something unexpected: the output pane is already
# reporting a missing binary, and a version invented on top of that would be
# worse than none.
proc crackNumVersion {} {
    global CRACKNUM
    if {$CRACKNUM eq ""} { return "" }
    if {[catch {exec $CRACKNUM -v 2>@1} out]} { return "" }
    # "crackNum v4.3, (c) Levent Erkok. Released with a BSD3 license."
    # \y, not \b: in Tcl's regexp \b is a backspace, so \b never matches here.
    if {[regexp {\yv(\d[\w.]*)} $out -> v]} { return $v }
    return ""
}

# Tk has no "open this in a browser", so hand off to the platform's opener.
# Returns 1 if something was launched, 0 if nothing could be.
#
# macOS and Windows each have exactly one answer. Linux has none: xdg-open is
# the convention but ships with the xdg-utils package, which a minimal install
# may simply not have -- and its absence is the whole reason this used to do
# nothing at all when clicked. So try the plausible openers in turn and report
# honestly when every one is missing, rather than swallowing it.
proc openURL {url} {
    set cmds {}

    if {$::tcl_platform(platform) eq "windows"} {
        lappend cmds [list {*}[auto_execok start] "" $url]
    } elseif {$::tcl_platform(os) eq "Darwin"} {
        lappend cmds [list open $url]
    } else {
        # $BROWSER first: if the user has said what they want, honour it.
        if {[info exists ::env(BROWSER)] && $::env(BROWSER) ne ""} {
            lappend cmds [list $::env(BROWSER) $url]
        }
        # firefox ahead of the indirection layers: naming a browser we can see
        # on PATH is one step, where xdg-open and gio each add a lookup that can
        # fail for its own reasons. The desktop-integration openers stay as
        # fallbacks for machines without firefox.
        foreach opener {firefox xdg-open gio gnome-open kde-open5 kde-open
                        x-www-browser sensible-browser chromium
                        chromium-browser google-chrome} {
            if {$opener eq "gio"} {
                # gio wants a subcommand, and is only useful if a handler is
                # actually registered: with none it exits non-zero *after* we
                # have backgrounded it, which reads as success here and would
                # stop us trying the browsers below -- another silent no-op.
                # Ask first, and simply skip gio when the answer is no.
                if {![catch {exec gio mime x-scheme-handler/https} reply]
                    && [string match -nocase "*default*:*" $reply]} {
                    lappend cmds [list gio open $url]
                }
            } else {
                lappend cmds [list $opener $url]
            }
        }
    }

    foreach cmd $cmds {
        # auto_execok rather than trusting exec to fail: a missing opener and a
        # browser that launched and then exited look the same to a backgrounded
        # exec, and only the first should send us on to the next candidate.
        if {[auto_execok [lindex $cmd 0]] eq ""} { continue }
        if {![catch {exec {*}$cmd &}]} { return 1 }
    }
    return 0
}

# Packed with -before .main: .main is packed with -expand yes and would otherwise
# have already claimed the space this bar needs, leaving it squeezed or invisible.
frame .footer
pack .footer -side bottom -fill x -padx 8 -pady {0 6} -before .main

set VERSION [crackNumVersion]
if {$VERSION ne ""} {
    label .footer.ver -text "crackNum v$VERSION" -anchor w
    pack  .footer.ver -side left
}

label .footer.link -text "Bugs/Feedback?" -anchor e \
      -fg #2b62e8 -cursor hand2
pack  .footer.link -side right

# Underline only the link, leaving the rest of the footer in the default font.
set linkFont [font actual [.footer.link cget -font]]
dict set linkFont -underline 1
.footer.link configure -font $linkFont

# If no browser could be launched, say so and show the URL: a deliberate click
# that produces nothing at all is indistinguishable from a broken widget, which
# is exactly how this first shipped.
bind .footer.link <Button-1> {
    if {![openURL $ISSUES_URL]} {
        tk_messageBox -parent . -icon info -title "crackNum" \
            -message "Could not find a browser to open:\n\n$ISSUES_URL" \
            -detail "Copy the address above, or install xdg-utils."
    }
}

# ---------------------------------------------------------------------------
# Start
# ---------------------------------------------------------------------------
showOutput $WELCOME
parseArgs $argv
syncCustomBox
if {$state(selection) ne ""} { crack }
focus .top.val
.top.val icursor end
