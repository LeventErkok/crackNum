using System;
using System.Diagnostics;
using System.Text;
using System.Text.RegularExpressions;

namespace CrackNumGUI
{
    /// <summary>
    /// What to make of whatever is sitting in the value box. An empty or whitespace-only
    /// box is not a value: this used to be defaulted to "0", which cracked a number the
    /// user had never typed and presented the result exactly like a real one, down to
    /// reporting the conversion as exact. Kept out of MainForm so --selftest can assert
    /// it headlessly; RunCrack itself is not reachable without a live form and Runner.
    /// </summary>
    internal sealed class ValueInput
    {
        /// <summary>Shown in place of a result when no value has been entered.</summary>
        internal const string Prompt = "Enter a value above to crack it.";

        internal bool HasValue { get; private set; }

        /// <summary>The text to hand to crackNum, verbatim. Null when there is none.</summary>
        internal string Value { get; private set; }

        private ValueInput(bool hasValue, string value)
        {
            HasValue = hasValue;
            Value = value;
        }

        internal static ValueInput For(string text)
        {
            // Passed through untouched when present: crackNum distinguishes 0xdeadbeef
            // (decode) from 3735928559 (encode), so any normalizing here would change
            // what the user asked for.
            return string.IsNullOrWhiteSpace(text) ? new ValueInput(false, null)
                                                   : new ValueInput(true, text);
        }
    }

    internal static class Runner
    {
        /// <summary>
        /// Run crackNum and return its combined stdout+stderr text.
        /// </summary>
        internal static string Run(string flag, string rounding, string value)
        {
            var crackNum = Tools.CrackNum;
            if (crackNum == null)
            {
                return "crackNum: Cannot locate 'crackNum.exe'." + Environment.NewLine + Environment.NewLine
                     + "It normally sits in the same folder as this program. Make sure the bundle was"
                     + " unpacked intact, or put crackNum.exe on your PATH.";
            }

            var z3 = Tools.Z3;
            if (z3 == null)
            {
                return "crackNum: Cannot locate 'z3.exe'." + Environment.NewLine + Environment.NewLine
                     + "crackNum shells out to the z3 solver for every operation. It ships in the same"
                     + " bundle; make sure it was unpacked intact, or put z3.exe on your PATH.";
            }

            // We never pass -l: crackNum infers the lane count from Verilog (N'h) input,
            // and everything else is a single lane.
            var args = new[] { flag, "-r" + rounding, "--", value };

            var psi = new ProcessStartInfo
            {
                FileName = crackNum,
                UseShellExecute = false,
                CreateNoWindow = true,          // otherwise every crack flashes a console
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                RedirectStandardInput = true,   // crackNum reads its value from argv, not stdin
            };

            foreach (var a in args)
            {
                psi.Arguments = psi.Arguments.Length == 0 ? Quote(a) : psi.Arguments + " " + Quote(a);
            }

            // SBV resolves the solver through this, which is what lets the bundled z3 be
            // found even when the user's PATH knows nothing about it. The Tcl GUI does
            // exactly the same thing for the same reason.
            psi.EnvironmentVariables["SBV_Z3"] = z3;

            var stdout = new StringBuilder();
            var stderr = new StringBuilder();

            try
            {
                using (var proc = new Process { StartInfo = psi })
                {
                    // Read both pipes asynchronously. Draining one and then the other
                    // deadlocks as soon as the child fills the pipe we are not reading.
                    proc.OutputDataReceived += (s, e) => { if (e.Data != null) { stdout.AppendLine(e.Data); } };
                    proc.ErrorDataReceived  += (s, e) => { if (e.Data != null) { stderr.AppendLine(e.Data); } };

                    proc.Start();
                    proc.BeginOutputReadLine();
                    proc.BeginErrorReadLine();
                    proc.StandardInput.Close();
                    proc.WaitForExit();

                    var result = stdout.ToString() + stderr.ToString();

                    if (proc.ExitCode != 0)
                    {
                        var cmd = crackNum + " " + psi.Arguments;
                        result += Environment.NewLine + Environment.NewLine
                               + "** Call to crackNum failed! Make sure the value makes sense for the chosen format."
                               + Environment.NewLine + "**"
                               + Environment.NewLine + "**   Run: " + cmd
                               + Environment.NewLine + "**"
                               + Environment.NewLine + "**   Value : " + value;
                    }

                    return result;
                }
            }
            catch (Exception ex)
            {
                return "Failed to launch crackNum: " + ex.Message;
            }
        }

        /// <summary>
        /// The version crackNum reports, e.g. "4.3", or null if it cannot be asked.
        /// </summary>
        /// <remarks>
        /// Read from the binary rather than carried here, so the footer cannot claim a
        /// version other than the one actually answering. Null rather than a guess when
        /// crackNum is missing: the output pane is already saying so, and a version
        /// invented on top of that would be worse than none. Computed once -- the value
        /// cannot change while the process is running.
        /// </remarks>
        internal static string Version => LazyVersion.Value;

        private static readonly Lazy<string> LazyVersion = new Lazy<string>(() =>
        {
            var crackNum = Tools.CrackNum;
            if (crackNum == null)
            {
                return null;
            }

            var psi = new ProcessStartInfo
            {
                FileName = crackNum,
                Arguments = "-v",
                UseShellExecute = false,
                CreateNoWindow = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
            };

            try
            {
                using (var proc = new Process { StartInfo = psi })
                {
                    proc.Start();
                    var text = proc.StandardOutput.ReadToEnd() + proc.StandardError.ReadToEnd();
                    proc.WaitForExit();
                    if (proc.ExitCode != 0)
                    {
                        return null;
                    }

                    return ParseVersion(text);
                }
            }
            catch (Exception)
            {
                return null;
            }
        });

        /// <summary>
        /// Pull the version out of what <c>crackNum -v</c> printed, or null.
        /// </summary>
        /// <remarks>
        /// Split out from the process plumbing so --selftest can assert it without a
        /// crackNum.exe to run, which is the same reason ValueInput lives outside
        /// MainForm. The banner reads "crackNum.exe v4.3, (c) Levent Erkok. ..." on
        /// Windows, where Main.hs deliberately prints the .exe name.
        /// </remarks>
        internal static string ParseVersion(string text)
        {
            if (string.IsNullOrEmpty(text))
            {
                return null;
            }

            var m = Regex.Match(text, @"\bv(\d[\w.]*)");
            return m.Success ? m.Groups[1].Value : null;
        }

        /// <summary>
        /// Quote one argument for the Windows command line.
        /// </summary>
        /// <remarks>
        /// Windows hands the child a single string and lets it do its own splitting, so
        /// values containing spaces or quotes have to be escaped here. This matters more
        /// than it looks: crackNum takes Verilog literals such as <c>19'h0000F</c>, and a
        /// bit pattern may legitimately carry spaces as separators.
        /// </remarks>
        private static string Quote(string arg)
        {
            if (arg.Length > 0 && arg.IndexOfAny(new[] { ' ', '\t', '"', '\\' }) < 0)
            {
                return arg;
            }

            var sb = new StringBuilder("\"");
            var backslashes = 0;

            foreach (var c in arg)
            {
                if (c == '\\')
                {
                    backslashes++;
                    continue;
                }

                if (c == '"')
                {
                    // Backslashes before a quote must themselves be doubled.
                    sb.Append('\\', (backslashes * 2) + 1);
                    backslashes = 0;
                    sb.Append('"');
                    continue;
                }

                sb.Append('\\', backslashes);
                backslashes = 0;
                sb.Append(c);
            }

            // Trailing backslashes would otherwise escape the closing quote.
            sb.Append('\\', backslashes * 2);
            sb.Append('"');
            return sb.ToString();
        }
    }
}
