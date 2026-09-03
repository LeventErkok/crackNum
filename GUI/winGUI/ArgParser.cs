using System;
using System.Collections.Generic;
using System.Globalization;

namespace CrackNumGUI
{
    /// <summary>Parsed crackNum-style command-line arguments.</summary>
    internal sealed class ParsedArgs
    {
        internal string Value;
        internal string FormatCode;
        internal int? BitWidth;
        internal int? ExpWidth;
        internal string Rounding;
    }

    internal static class ArgParser
    {
        /// <summary>
        /// Parse the crackNum flags forwarded by <c>crackNum --gui ...</c>: -f&lt;fmt&gt;,
        /// -w&lt;N&gt;, -i&lt;N&gt;, -r&lt;mode&gt; in their attached form (-fsp, -w32, -rRTZ),
        /// plus <c>--</c> to introduce a value that begins with '-'.
        /// </summary>
        /// <remarks>
        /// crackNum's Haskell front-end has already parsed and validated these, so we
        /// only handle the canonical attached forms; any other flag (-l lanes, -d, -v,
        /// ...) is ignored.
        /// </remarks>
        internal static ParsedArgs Parse(IList<string> args)
        {
            var p = new ParsedArgs();
            var values = new List<string>();

            for (var i = 0; i < args.Count; i++)
            {
                var a = args[i];

                if (a == "--")
                {
                    // Everything after this is a value (e.g. a negative number).
                    for (var j = i + 1; j < args.Count; j++)
                    {
                        values.Add(args[j]);
                    }

                    break;
                }

                if (a.StartsWith("-f", StringComparison.Ordinal))
                {
                    ApplyFloat(p, a.Substring(2).ToLowerInvariant());
                }
                else if (a.StartsWith("-w", StringComparison.Ordinal))
                {
                    ApplyInteger(p, true, a.Substring(2));
                }
                else if (a.StartsWith("-i", StringComparison.Ordinal))
                {
                    ApplyInteger(p, false, a.Substring(2));
                }
                else if (a.StartsWith("-r", StringComparison.Ordinal))
                {
                    var rm = a.Substring(2).ToUpperInvariant();
                    if (Array.IndexOf(Formats.RoundingModes, rm) >= 0)
                    {
                        p.Rounding = rm;
                    }
                }
                else if (a.StartsWith("-", StringComparison.Ordinal) && a != "-")
                {
                    // Some other flag (-l lanes, -d, --debug, -v, ...): ignore.
                }
                else
                {
                    values.Add(a);
                }
            }

            if (values.Count > 0)
            {
                p.Value = string.Join(" ", values.ToArray());
            }

            return p;
        }

        private static void ApplyFloat(ParsedArgs p, string v)
        {
            switch (v)
            {
                case "sp":      p.FormatCode = "fsp";      return;
                case "dp":      p.FormatCode = "fdp";      return;
                case "qp":      p.FormatCode = "fqp";      return;
                case "hp":      p.FormatCode = "fhp";      return;
                case "bp":      p.FormatCode = "fbp";      return;
                case "tf32":    p.FormatCode = "ftf32";    return;
                case "e4m3":    p.FormatCode = "fe4m3";    return;
                case "e5m2":    p.FormatCode = "fe5m2";    return;
                case "fp4":     p.FormatCode = "ffp4";     return;
                case "fp4e0m3": p.FormatCode = "ffp4e0m3"; return;
                case "e8m0":    p.FormatCode = "fe8m0";    return;
                case "ue5m3":   p.FormatCode = "fue5m3";   return;
            }

            // Custom "E+S": E exponent bits, S significand bits (incl. implicit).
            var parts = v.Split(new[] { '+' }, 2);
            if (parts.Length == 2
                && int.TryParse(parts[0], NumberStyles.Integer, CultureInfo.InvariantCulture, out var e)
                && int.TryParse(parts[1], NumberStyles.Integer, CultureInfo.InvariantCulture, out var s))
            {
                p.FormatCode = "fcs";
                p.ExpWidth = e;
                // Total width = 1 sign + E + (S-1) stored = E + S
                p.BitWidth = e + s;
            }
        }

        private static void ApplyInteger(ParsedArgs p, bool unsigned, string v)
        {
            var prefix = unsigned ? "w" : "i";

            if (v == "8" || v == "16" || v == "32" || v == "64")
            {
                p.FormatCode = prefix + v;
                return;
            }

            if (int.TryParse(v, NumberStyles.Integer, CultureInfo.InvariantCulture, out var n) && n > 0)
            {
                p.FormatCode = prefix + "cs";   // wcs / ics (custom width)
                p.BitWidth = n;
            }
        }
    }
}
