using System;
using System.Collections.Generic;
using System.Drawing;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace CrackNumGUI
{
    internal static class Program
    {
        // A WinExe has no console of its own. When --selftest is run from a shell we
        // want its report to land in that shell (and hence in the CI log), so attach
        // to the parent's console if there is one. Harmless when there isn't.
        [DllImport("kernel32.dll")]
        private static extern bool AttachConsole(int processId);

        private const int AttachParentProcess = -1;

        [STAThread]
        private static int Main(string[] argv)
        {
            var args = new List<string>(argv);

            if (args.Contains("--selftest"))
            {
                AttachConsole(AttachParentProcess);
                return SelfTest();
            }

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            // Parse the crackNum-style arguments forwarded by `crackNum --gui ...`
            // (also works when running this binary directly, e.g. CrackNumGUI -fsp 2.5).
            Application.Run(new MainForm(ArgParser.Parse(args)));
            return 0;
        }

        /// <summary>
        /// Headless smoke test: exercises everything that does not need a user.
        /// </summary>
        /// <remarks>
        /// This exists because there is no Windows machine in the loop -- CI is the only
        /// place this code ever runs. The compiler catches type errors; this catches the
        /// next tier up: a format whose flag cannot be built, an argument form that stops
        /// round-tripping, and above all a window whose construction throws, which is the
        /// classic way a WinForms layout breaks. It deliberately does NOT invoke
        /// crackNum.exe: the point is to be runnable before the bundle is assembled.
        /// </remarks>
        private static int SelfTest()
        {
            var failures = new List<string>();

            // 1. Every format must yield a precision flag under the default widths.
            foreach (var section in Formats.Sections)
            {
                foreach (var fmt in section.Formats)
                {
                    var res = FlagResult.For(fmt, 64, 11);
                    if (!res.IsValid)
                    {
                        failures.Add("format " + fmt.Id + " (" + section.Title + ") produced no flag: " + res.Invalid);
                    }
                    else if (string.IsNullOrEmpty(res.Flag) || res.Flag[0] != '-')
                    {
                        failures.Add("format " + fmt.Id + " produced a malformed flag: " + res.Flag);
                    }
                }
            }

            // 2. A custom float with an impossible layout must be rejected, not crash.
            if (FlagResult.For(Formats.ById("fcs"), 4, 0).IsValid)
            {
                failures.Add("custom float accepted a 0-bit exponent");
            }

            // 3. Argument parsing round-trips the forms crackNum --gui actually sends.
            CheckParse(failures, new[] { "-fsp", "-rRNE", "--", "2.5" }, "fsp", "2.5", "RNE");
            CheckParse(failures, new[] { "-fe8m0", "-rRNE", "--", "0xFE" }, "fe8m0", "0xFE", "RNE");
            CheckParse(failures, new[] { "-fue5m3", "-rRNE", "--", "0xFE" }, "fue5m3", "0xFE", "RNE");
            CheckParse(failures, new[] { "-w32", "--", "0xdeadbeef" }, "w32", "0xdeadbeef", null);
            CheckParse(failures, new[] { "-i16", "-rRTZ", "--", "-42" }, "i16", "-42", "RTZ");
            CheckParse(failures, new[] { "-f8+24", "--", "1.5" }, "fcs", "1.5", null);

            // 3b. The shared custom-width box must describe what it actually drives:
            //     the heading names the selected format, and the exponent row is live
            //     only for the float. Layout bugs do not fail builds, so assert the
            //     mapping here rather than trusting the screenshot to show it.
            CheckCustomBox(failures, "fcs", "Custom IEEE-754 float:", true, true);
            CheckCustomBox(failures, "ics", "Custom signed integer:", true, false);
            CheckCustomBox(failures, "wcs", "Custom unsigned word:", true, false);
            CheckCustomBox(failures, "fsp", "Custom format:", false, false);
            CheckCustomBox(failures, "i32", "Custom format:", false, false);
            CheckCustomBox(failures, null, "Custom format:", false, false);

            // 3c. An empty value box must never be turned into a number. It used to
            //     default to "0", which cracked a value the user had never typed and
            //     rendered it exactly like a real result. Anything present must reach
            //     crackNum byte-for-byte, since 0xdeadbeef and 3735928559 mean different
            //     operations.
            CheckValueInput(failures, null, false);
            CheckValueInput(failures, "", false);
            CheckValueInput(failures, "   ", false);
            CheckValueInput(failures, "\t", false);
            CheckValueInput(failures, "0", true);
            CheckValueInput(failures, "2.5", true);
            CheckValueInput(failures, "0xdeadbeef", true);

            // 4. The embedded icon must actually be there under the name MainForm
            //    looks it up by. MainForm deliberately falls back to the stock icon
            //    rather than throwing, so without this check a rename or a dropped
            //    EmbeddedResource entry would ship the wrong icon with CI still green.
            try
            {
                var iconName = typeof(MainForm).Namespace + ".CrackNum.ico";
                using (var stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(iconName))
                {
                    if (stream == null)
                    {
                        failures.Add("embedded icon not found: " + iconName + " (have: "
                                   + string.Join(", ", Assembly.GetExecutingAssembly().GetManifestResourceNames()) + ")");
                    }
                    else
                    {
                        using (var icon = new Icon(stream))
                        {
                            if (icon.Width == 0)
                            {
                                failures.Add("embedded icon loaded but has zero width");
                            }
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                failures.Add("loading the embedded icon threw: " + ex.Message);
            }

            // 4b. Version parsing must survive whatever -v prints, and must decline
            //     rather than guess when there is no version in the text at all.
            var versionCases = new[]
            {
                new { Text = "crackNum.exe v4.3, (c) Levent Erkok. Released with a BSD3 license.", Want = "4.3" },
                new { Text = "crackNum v4.3, (c) Levent Erkok. Released with a BSD3 license.",     Want = "4.3" },
                new { Text = "crackNum v10.12.1, (c) x",                                          Want = "10.12.1" },
                new { Text = "no version here",                                                   Want = (string)null },
                new { Text = "",                                                                  Want = (string)null },
                new { Text = (string)null,                                                        Want = (string)null },
            };
            foreach (var c in versionCases)
            {
                var got = Runner.ParseVersion(c.Text);
                if (got != c.Want)
                {
                    failures.Add("ParseVersion(" + (c.Text ?? "<null>") + ") gave "
                                 + (got ?? "<null>") + ", wanted " + (c.Want ?? "<null>"));
                }
            }

            // 5. The window must build. This is the one that catches layout mistakes,
            //    so force handle creation rather than settling for the constructor.
            try
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                using (var form = new MainForm(new ParsedArgs()))
                {
                    form.CreateControl();
                    if (form.Handle == IntPtr.Zero)
                    {
                        failures.Add("main window did not get a handle");
                    }
                }
            }
            catch (Exception ex)
            {
                failures.Add("constructing the main window threw: " + ex);
            }

            if (failures.Count == 0)
            {
                Console.WriteLine("selftest: OK");
                return 0;
            }

            Console.WriteLine("selftest: " + failures.Count + " failure(s)");
            foreach (var f in failures)
            {
                Console.WriteLine("  - " + f);
            }

            return 1;
        }

        private static void CheckValueInput(ICollection<string> failures, string text, bool expected)
        {
            var label = text == null ? "(null)" : "\"" + text + "\"";
            var input = ValueInput.For(text);

            if (input.HasValue != expected)
            {
                failures.Add("value box " + label + " gave HasValue=" + input.HasValue + ", expected " + expected);
            }

            if (input.HasValue && input.Value != text)
            {
                failures.Add("value box " + label + " was altered to \"" + input.Value + "\" before being passed on");
            }
        }

        private static void CheckCustomBox(ICollection<string> failures, string id, string heading, bool width, bool exponent)
        {
            var label = id ?? "(no selection)";
            var box = CustomBox.For(Formats.ById(id));

            if (box.Heading != heading)
            {
                failures.Add("custom box for " + label + " is headed \"" + box.Heading + "\", expected \"" + heading + "\"");
            }

            if (box.WidthApplies != width)
            {
                failures.Add("custom box for " + label + " has WidthApplies=" + box.WidthApplies + ", expected " + width);
            }

            if (box.ExponentApplies != exponent)
            {
                failures.Add("custom box for " + label + " has ExponentApplies=" + box.ExponentApplies + ", expected " + exponent);
            }
        }

        private static void CheckParse(ICollection<string> failures, string[] args, string format, string value, string rounding)
        {
            var p = ArgParser.Parse(args);
            var shown = string.Join(" ", args);

            if (p.FormatCode != format)
            {
                failures.Add(shown + ": expected format " + format + ", got " + (p.FormatCode ?? "<none>"));
            }

            if (p.Value != value)
            {
                failures.Add(shown + ": expected value " + value + ", got " + (p.Value ?? "<none>"));
            }

            if (rounding != null && p.Rounding != rounding)
            {
                failures.Add(shown + ": expected rounding " + rounding + ", got " + (p.Rounding ?? "<none>"));
            }
        }
    }
}
