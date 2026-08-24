using System;
using System.IO;
using System.Reflection;

namespace CrackNumGUI
{
    /// <summary>
    /// Locates the crackNum and z3 executables.
    /// </summary>
    /// <remarks>
    /// Two tiers, mirroring the Tcl GUI's <c>locate</c> proc:
    ///
    ///   1. Next to this executable. That is how the release zip is laid out, and it
    ///      is what makes the bundle work when the user has not touched their PATH.
    ///   2. The PATH, for a setup that keeps the tools somewhere else.
    ///
    /// Note there is deliberately no equivalent of the macOS GUI's second tier, where
    /// it asks an interactive login shell for the real PATH. That exists because a
    /// Finder-launched .app inherits a stripped-down environment; a Windows GUI app
    /// started from Explorer inherits the user's full environment already, so the
    /// question does not arise.
    /// </remarks>
    internal static class Tools
    {
        /// <summary>Directory holding this executable.</summary>
        internal static string AppDir
        {
            get
            {
                // Assembly location rather than Application.StartupPath: this type is
                // reachable from --selftest, which must not depend on WinForms being
                // initialized.
                var path = Assembly.GetExecutingAssembly().Location;
                var dir = Path.GetDirectoryName(path);
                return string.IsNullOrEmpty(dir) ? Environment.CurrentDirectory : dir;
            }
        }

        private static string Locate(string exeName)
        {
            var beside = Path.Combine(AppDir, exeName);
            if (File.Exists(beside))
            {
                return beside;
            }

            var path = Environment.GetEnvironmentVariable("PATH");
            if (string.IsNullOrEmpty(path))
            {
                return null;
            }

            foreach (var raw in path.Split(Path.PathSeparator))
            {
                // PATH entries are sometimes quoted, and a malformed one (stray '|',
                // say) would otherwise take the whole lookup down with an exception.
                var dir = raw.Trim().Trim('"');
                if (dir.Length == 0)
                {
                    continue;
                }

                string candidate;
                try
                {
                    candidate = Path.Combine(dir, exeName);
                }
                catch (ArgumentException)
                {
                    continue;
                }

                if (File.Exists(candidate))
                {
                    return candidate;
                }
            }

            return null;
        }

        private static readonly Lazy<string> LazyCrackNum = new Lazy<string>(() => Locate("crackNum.exe"));
        private static readonly Lazy<string> LazyZ3 = new Lazy<string>(() => Locate("z3.exe"));

        /// <summary>Full path to crackNum.exe, or null if it cannot be found.</summary>
        internal static string CrackNum => LazyCrackNum.Value;

        /// <summary>Full path to z3.exe, or null if it cannot be found.</summary>
        internal static string Z3 => LazyZ3.Value;
    }
}
