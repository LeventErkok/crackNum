using System;
using System.Collections.Generic;
using System.Globalization;

namespace CrackNumGUI
{
    internal enum FormatKind
    {
        FixedFloat,
        CustomFloat,
        FixedWord,
        CustomWord,
        FixedInt,
        CustomInt
    }

    internal sealed class Format
    {
        internal string Id { get; }         // stable code, e.g. "fsp"
        internal string Label { get; }      // shown in the sidebar
        internal FormatKind Kind { get; }
        internal string Flag { get; }       // for FixedFloat, e.g. "fsp"
        internal int Bits { get; }          // for FixedWord / FixedInt

        internal Format(string id, string label, FormatKind kind, string flag = null, int bits = 0)
        {
            Id = id;
            Label = label;
            Kind = kind;
            Flag = flag;
            Bits = bits;
        }
    }

    internal sealed class FormatSection
    {
        internal string Title { get; }
        internal IList<Format> Formats { get; }

        internal FormatSection(string title, IList<Format> formats)
        {
            Title = title;
            Formats = formats;
        }
    }

    internal static class Formats
    {
        // The floats are grouped by provenance rather than by width: first the formats
        // that exist because of machine learning (the narrow FP4/FP8 ones, plus bfloat16
        // and TF32), then the IEEE-754 ones. Order here is the order the sidebar shows.
        // The ids are what ArgParser maps -f/-w/-i onto, so they stay put even when a
        // format moves from one group to another.
        //
        // This table is the one thing duplicated across all three GUIs (Swift, Tcl, and
        // here). Keep the three in step.
        internal static readonly IList<FormatSection> Sections = new List<FormatSection>
        {
            new FormatSection("AI formats", new List<Format>
            {
                new Format("ffp4",     "FP4 (E2M1)", FormatKind.FixedFloat, flag: "ffp4"),
                new Format("ffp4e0m3", "FP4 (E0M3)", FormatKind.FixedFloat, flag: "ffp4e0m3"),
                new Format("fe4m3",    "FP8 (E4M3)", FormatKind.FixedFloat, flag: "fe4m3"),
                new Format("fe5m2",    "FP8 (E5M2)", FormatKind.FixedFloat, flag: "fe5m2"),
                new Format("fe8m0",    "FP8 (E8M0)", FormatKind.FixedFloat, flag: "fe8m0"),
                new Format("fbp",      "Brain",      FormatKind.FixedFloat, flag: "fbp"),
                new Format("ftf32",    "TF32",       FormatKind.FixedFloat, flag: "ftf32"),
            }),
            new FormatSection("IEEE-754", new List<Format>
            {
                new Format("fhp",      "Half",       FormatKind.FixedFloat, flag: "fhp"),
                new Format("fsp",      "Single",     FormatKind.FixedFloat, flag: "fsp"),
                new Format("fdp",      "Double",     FormatKind.FixedFloat, flag: "fdp"),
                new Format("fqp",      "Quad",       FormatKind.FixedFloat, flag: "fqp"),
                new Format("fcs",      "Custom",     FormatKind.CustomFloat),
            }),
            new FormatSection("Integer (Signed)", new List<Format>
            {
                new Format("i8",  "8-bit",  FormatKind.FixedInt, bits: 8),
                new Format("i16", "16-bit", FormatKind.FixedInt, bits: 16),
                new Format("i32", "32-bit", FormatKind.FixedInt, bits: 32),
                new Format("i64", "64-bit", FormatKind.FixedInt, bits: 64),
                new Format("ics", "Custom", FormatKind.CustomInt),
            }),
            new FormatSection("Word (Unsigned)", new List<Format>
            {
                new Format("w8",  "8-bit",  FormatKind.FixedWord, bits: 8),
                new Format("w16", "16-bit", FormatKind.FixedWord, bits: 16),
                new Format("w32", "32-bit", FormatKind.FixedWord, bits: 32),
                new Format("w64", "64-bit", FormatKind.FixedWord, bits: 64),
                new Format("wcs", "Custom", FormatKind.CustomWord),
            }),
        };

        internal static readonly string[] RoundingModes = { "RNE", "RNA", "RTP", "RTN", "RTZ" };

        // NB. The infinity signs are \u escapes rather than literal UTF-8. The C#
        // compiler would handle either, but keeping them escaped means the file is
        // pure ASCII and cannot be mangled by an editor or a checkout with a
        // different encoding.
        internal static readonly IDictionary<string, string> RoundingLabels = new Dictionary<string, string>
        {
            { "RNE", "RNE (Nearest, ties to even)" },
            { "RNA", "RNA (Nearest, ties to away)" },
            { "RTP", "RTP (Toward +\u221E)" },
            { "RTN", "RTN (Toward -\u221E)" },
            { "RTZ", "RTZ (Toward 0)" },
        };

        internal static Format ById(string id)
        {
            if (id == null)
            {
                return null;
            }

            foreach (var section in Sections)
            {
                foreach (var fmt in section.Formats)
                {
                    if (fmt.Id == id)
                    {
                        return fmt;
                    }
                }
            }

            return null;
        }
    }

    /// <summary>Either a crackNum precision flag, or a message explaining why there isn't one.</summary>
    /// <summary>
    /// What the shared custom-width box is driving for a given selection. The box
    /// serves all three "Custom" entries -- IEEE-754 float, signed integer, and
    /// unsigned word -- but only the float has an exponent, and with a fixed format
    /// selected nothing in the box does anything at all. Kept out of MainForm so the
    /// mapping can be asserted headlessly by --selftest.
    /// </summary>
    internal sealed class CustomBox
    {
        internal string Heading { get; private set; }
        internal bool WidthApplies { get; private set; }
        internal bool ExponentApplies { get; private set; }

        private CustomBox(string heading, bool widthApplies, bool exponentApplies)
        {
            Heading = heading;
            WidthApplies = widthApplies;
            ExponentApplies = exponentApplies;
        }

        internal static CustomBox For(Format fmt)
        {
            if (fmt != null)
            {
                switch (fmt.Kind)
                {
                    case FormatKind.CustomFloat: return new CustomBox("Custom IEEE-754 float:", true, true);
                    case FormatKind.CustomInt:   return new CustomBox("Custom signed integer:", true, false);
                    case FormatKind.CustomWord:  return new CustomBox("Custom unsigned word:",  true, false);
                }
            }

            return new CustomBox("Custom format:", false, false);
        }
    }

    internal sealed class FlagResult
    {
        internal string Flag { get; }
        internal string Invalid { get; }
        internal bool IsValid => Invalid == null;

        private FlagResult(string flag, string invalid)
        {
            Flag = flag;
            Invalid = invalid;
        }

        internal static FlagResult Valid(string flag) => new FlagResult(flag, null);
        internal static FlagResult Bad(string message) => new FlagResult(null, message);

        /// <summary>Build the crackNum precision flag for a format, given the custom widths.</summary>
        internal static FlagResult For(Format fmt, int bitWidth, int expWidth)
        {
            switch (fmt.Kind)
            {
                case FormatKind.FixedFloat:
                    return Valid("-" + fmt.Flag);

                case FormatKind.CustomFloat:
                    // Only check that the widths describe a well-formed layout; crackNum
                    // itself owns the remaining limits (and reports solver restrictions
                    // readably).
                    var sigWidth = bitWidth - expWidth - 1;
                    if (expWidth < 1 || sigWidth < 0)
                    {
                        return Bad(string.Format(
                            CultureInfo.InvariantCulture,
                            "Invalid custom FP format:{0}" +
                            "  Total width: {1}{0}" +
                            "    Sign       :    1{0}" +
                            "    Exponent   : {2,4}{0}" +
                            "    Significand: {3,4} (Total = Sign + Exponent + Significand){0}" +
                            "{0}" +
                            "Exponent must be at least 1 bit, and the total width must leave room for it and the sign.",
                            Environment.NewLine, bitWidth, expWidth, sigWidth));
                    }

                    // crackNum's -fE+S: E exponent bits, S significand bits *including*
                    // the implied bit.
                    return Valid(string.Format(CultureInfo.InvariantCulture, "-f{0}+{1}", expWidth, bitWidth - expWidth));

                case FormatKind.FixedWord:
                    return Valid("-w" + fmt.Bits.ToString(CultureInfo.InvariantCulture));

                case FormatKind.CustomWord:
                    return Valid("-w" + bitWidth.ToString(CultureInfo.InvariantCulture));

                case FormatKind.FixedInt:
                    return Valid("-i" + fmt.Bits.ToString(CultureInfo.InvariantCulture));

                case FormatKind.CustomInt:
                    return Valid("-i" + bitWidth.ToString(CultureInfo.InvariantCulture));

                default:
                    return Bad("Unknown format kind: " + fmt.Kind);
            }
        }
    }
}
