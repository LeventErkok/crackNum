using System;
using System.Collections.Generic;
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
            CheckParse(failures, new[] { "-w32", "--", "0xdeadbeef" }, "w32", "0xdeadbeef", null);
            CheckParse(failures, new[] { "-i16", "-rRTZ", "--", "-42" }, "i16", "-42", "RTZ");
            CheckParse(failures, new[] { "-f8+24", "--", "1.5" }, "fcs", "1.5", null);

            // 4. The window must build. This is the one that catches layout mistakes,
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
