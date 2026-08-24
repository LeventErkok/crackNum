using System;
using System.Diagnostics;
using System.Text;

namespace CrackNumGUI
{
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
