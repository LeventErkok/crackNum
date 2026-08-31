using System;
using System.Diagnostics;
using System.Drawing;
using System.Globalization;
using System.Reflection;
using System.Windows.Forms;

namespace CrackNumGUI
{
    internal sealed class MainForm : Form
    {
        /// <summary>
        /// Where the GUIs send bug reports. Same destination in the Swift, Tcl, Windows
        /// and web front ends, so a report lands in the same place whichever one is used.
        /// </summary>
        private const string RepoUrl = "https://github.com/LeventErkok/crackNum";
        private const string IssuesUrl = RepoUrl + "/issues";

        private const string Welcome =
            "Enter a value above, then pick a format on the left to crack it.\n" +
            "\n" +
            "You can:\n" +
            "  - ENCODE: from a mathematical value to its internal representation\n" +
            "  - DECODE: from an internal representation to its mathematical value\n" +
            "\n" +
            "Encoding:\n" +
            "  - Enter a decimal value (2.5, -4.1e5) or hex float (0x2.4p3).\n" +
            "  - You can pass NaN, Inf, -0, -Inf for special values.\n" +
            "  - For floats, pick a rounding mode.\n" +
            "  - Input must NOT start with 0x, 0b, or N'h (else we decode instead).\n" +
            "\n" +
            "Decoding:\n" +
            "  - Use hex (0x), binary (0b), or Verilog (N'h) notation.\n" +
            "  - You may use _, - or space as separators for readability.\n" +
            "  - Verilog N'h: N is the total width, split into N/format-size lanes.";

        private readonly TreeView _formats = new TreeView();
        private readonly TextBox _value = new TextBox();
        private readonly TextBox _output = new TextBox();
        private readonly ComboBox _rounding = new ComboBox();
        private readonly TextBox _bitWidth = new TextBox();
        private readonly TextBox _expWidth = new TextBox();

        // Held rather than built inline: SyncCustomBox retitles the heading and greys
        // the rows that the selected format does not use, so it needs all three.
        private readonly Label _customHeading = new Label();
        private readonly Label _bitWidthLabel = MakeFieldLabel("Total width:");
        private readonly Label _expWidthLabel = MakeFieldLabel("Exponent width:");

        // Held as a field on purpose: a ToolTip that nothing references gets collected,
        // and the tips silently stop appearing.
        private readonly ToolTip _tips = new ToolTip();

        private float _fontSize = 10f;
        private string _selection;

        internal MainForm(ParsedArgs parsed)
        {
            Text = "CrackNum";
            LoadIcon();
            ClientSize = new Size(1000, 620);
            MinimumSize = new Size(860, 600);
            StartPosition = FormStartPosition.CenterScreen;

            BuildUi();

            if (parsed.Value != null)    { _value.Text = parsed.Value; }
            if (parsed.BitWidth.HasValue) { _bitWidth.Text = parsed.BitWidth.Value.ToString(CultureInfo.InvariantCulture); }
            if (parsed.ExpWidth.HasValue) { _expWidth.Text = parsed.ExpWidth.Value.ToString(CultureInfo.InvariantCulture); }
            if (parsed.Rounding != null)  { _rounding.SelectedItem = parsed.Rounding; }

            SetOutput(Welcome);

            // If a format was supplied, select it and crack immediately, so the window
            // opens showing results rather than the welcome text.
            if (parsed.FormatCode != null)
            {
                SelectFormat(parsed.FormatCode);
            }
            else
            {
                SyncCustomBox();
            }
        }

        /// <summary>Set the window icon from the copy embedded in this assembly.</summary>
        /// <remarks>
        /// Deliberately non-fatal. An icon is decoration, and --selftest builds this
        /// form to check the layout; a missing or malformed resource should not be
        /// the thing that fails a release build.
        /// </remarks>
        private void LoadIcon()
        {
            try
            {
                var name = typeof(MainForm).Namespace + ".CrackNum.ico";
                using (var stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(name))
                {
                    if (stream != null)
                    {
                        Icon = new Icon(stream);
                    }
                }
            }
            catch (Exception)
            {
                // Keep the stock WinForms icon.
            }
        }

        private void BuildUi()
        {
            var root = new TableLayoutPanel
            {
                Dock = DockStyle.Fill,
                ColumnCount = 1,
                RowCount = 3,
                Padding = new Padding(8),
            };
            root.RowStyles.Add(new RowStyle(SizeType.AutoSize));
            root.RowStyles.Add(new RowStyle(SizeType.Percent, 100f));
            root.RowStyles.Add(new RowStyle(SizeType.AutoSize));
            Controls.Add(root);

            root.Controls.Add(BuildTopBar(), 0, 0);
            root.Controls.Add(BuildContent(), 0, 1);
            root.Controls.Add(BuildFooter(), 0, 2);
        }

        /// <summary>
        /// Version, and a way to report what it got wrong. The version label is left out
        /// entirely when crackNum could not be asked, rather than showing a placeholder.
        /// </summary>
        private Control BuildFooter()
        {
            // A table rather than a FlowLayoutPanel: flow cannot push one child to the
            // far edge. The first column takes all the slack, so the link in the second
            // sits hard right whether or not there is a version label beside it.
            var footer = new TableLayoutPanel
            {
                Dock = DockStyle.Fill,
                ColumnCount = 2,
                RowCount = 1,
                AutoSize = true,
                AutoSizeMode = AutoSizeMode.GrowAndShrink,
                Margin = new Padding(0, 6, 0, 0),
            };
            footer.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100f));
            footer.ColumnStyles.Add(new ColumnStyle(SizeType.AutoSize));

            var version = Runner.Version;
            if (version != null)
            {
                var versionLink = new LinkLabel
                {
                    Text = "crackNum v" + version,
                    AutoSize = true,
                    Anchor = AnchorStyles.Left,
                    Margin = new Padding(0, 3, 0, 0),
                };
                versionLink.LinkClicked += (s, e) => OpenUrl(RepoUrl);
                footer.Controls.Add(versionLink, 0, 0);
            }

            var link = new LinkLabel
            {
                Text = "Bugs/Feedback?",
                AutoSize = true,
                Anchor = AnchorStyles.Right,
                Margin = new Padding(0, 3, 0, 0),
            };
            link.LinkClicked += (s, e) => OpenUrl(IssuesUrl);
            footer.Controls.Add(link, 1, 0);

            return footer;
        }

        /// <summary>
        /// Open the issue tracker in the user's browser. UseShellExecute is what hands
        /// the URL to the shell to resolve; without it this would be an attempt to
        /// execute the string as a program. Failure is reported rather than thrown:
        /// losing the whole GUI because a browser would not start is a poor trade.
        /// </summary>
        private void OpenUrl(string url)
        {
            try
            {
                Process.Start(new ProcessStartInfo
                {
                    FileName = url,
                    UseShellExecute = true,
                });
            }
            catch (Exception ex)
            {
                MessageBox.Show(this,
                    "Could not open a browser." + Environment.NewLine + Environment.NewLine
                        + url + Environment.NewLine + Environment.NewLine + ex.Message,
                    "crackNum", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        private Control BuildTopBar()
        {
            var bar = new TableLayoutPanel
            {
                Dock = DockStyle.Fill,
                ColumnCount = 4,
                RowCount = 1,
                AutoSize = true,
                AutoSizeMode = AutoSizeMode.GrowAndShrink,
                Margin = new Padding(0, 0, 0, 8),
            };
            bar.ColumnStyles.Add(new ColumnStyle(SizeType.AutoSize));           // buttons
            bar.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 40f));       // spacer
            bar.ColumnStyles.Add(new ColumnStyle(SizeType.AutoSize));           // "Value"
            bar.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 60f));       // entry

            // WrapContents off: with it on, the panel is free to satisfy an AutoSize
            // width by folding the later buttons onto rows of their own, which then
            // sit outside the row's height. The result is a toolbar showing only its
            // first button.
            var buttons = new FlowLayoutPanel
            {
                AutoSize = true,
                AutoSizeMode = AutoSizeMode.GrowAndShrink,
                WrapContents = false,
                FlowDirection = FlowDirection.LeftToRight,
                Margin = new Padding(0),
            };
            buttons.Controls.Add(MakeButton("-", "Smaller output text", (s, e) => Zoom(-1)));
            buttons.Controls.Add(MakeButton("+", "Larger output text", (s, e) => Zoom(+1)));
            buttons.Controls.Add(MakeButton("?", "Show the usage summary", (s, e) => SetOutput(Welcome)));
            bar.Controls.Add(buttons, 0, 0);

            bar.Controls.Add(new Label(), 1, 0);

            var lbl = new Label
            {
                Text = "Value",
                AutoSize = true,
                TextAlign = ContentAlignment.MiddleRight,
                Anchor = AnchorStyles.Right,
                Margin = new Padding(3, 8, 6, 3),
            };
            bar.Controls.Add(lbl, 2, 0);

            _value.Dock = DockStyle.Fill;
            _value.Font = new Font("Consolas", 11f);
            _value.Margin = new Padding(0, 4, 0, 3);
            // Enter cracks, rather than dinging the way an unhandled Enter does in a
            // single-line TextBox.
            _value.KeyDown += OnEnterRun;
            bar.Controls.Add(_value, 3, 0);

            return bar;
        }

        private Control BuildContent()
        {
            var content = new TableLayoutPanel
            {
                Dock = DockStyle.Fill,
                ColumnCount = 2,
                RowCount = 1,
            };
            content.ColumnStyles.Add(new ColumnStyle(SizeType.Absolute, 280f));
            content.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100f));

            content.Controls.Add(BuildSidebar(), 0, 0);

            _output.Dock = DockStyle.Fill;
            _output.Multiline = true;
            _output.ReadOnly = true;
            _output.WordWrap = false;
            _output.ScrollBars = ScrollBars.Both;
            _output.BackColor = SystemColors.Window;
            _output.Font = new Font("Consolas", _fontSize);
            _output.Margin = new Padding(8, 0, 0, 0);
            content.Controls.Add(_output, 1, 0);

            return content;
        }

        private Control BuildSidebar()
        {
            var side = new TableLayoutPanel
            {
                Dock = DockStyle.Fill,
                ColumnCount = 1,
                RowCount = 3,
                Margin = new Padding(0),
            };
            side.RowStyles.Add(new RowStyle(SizeType.Percent, 100f));
            side.RowStyles.Add(new RowStyle(SizeType.AutoSize));
            side.RowStyles.Add(new RowStyle(SizeType.AutoSize));

            // A TreeView rather than a ListBox: the sidebar is grouped, and a TreeView
            // gives section headings for free. The lines and expanders are turned off
            // and collapsing is refused, so it reads as a sectioned list rather than
            // as a tree the user is meant to fold.
            _formats.Dock = DockStyle.Fill;
            _formats.ShowLines = false;
            _formats.ShowPlusMinus = false;
            _formats.ShowRootLines = false;
            _formats.HideSelection = false;
            _formats.FullRowSelect = true;
            _formats.ItemHeight = 20;
            _formats.Indent = 12;
            _formats.BorderStyle = BorderStyle.FixedSingle;

            // A TreeView measures how wide a node needs to be using the CONTROL's font,
            // never the per-node NodeFont. Giving the headings a bold NodeFont over a
            // regular control font therefore clips them -- "AI formats" renders as
            // "AI formal". Make the control's font the bold one, so measurement is
            // generous, and put the regular font back on the leaves.
            var regular = _formats.Font;
            var bold = new Font(regular, FontStyle.Bold);
            _formats.Font = bold;
            _formats.BeforeCollapse += (s, e) => e.Cancel = true;
            _formats.AfterSelect += OnFormatSelected;

            foreach (var section in Formats.Sections)
            {
                var head = new TreeNode(section.Title)
                {
                    NodeFont = bold,
                    ForeColor = SystemColors.GrayText,
                };

                foreach (var fmt in section.Formats)
                {
                    head.Nodes.Add(new TreeNode(fmt.Label) { Tag = fmt, NodeFont = regular });
                }

                _formats.Nodes.Add(head);
            }

            _formats.ExpandAll();
            side.Controls.Add(_formats, 0, 0);

            // Rounding mode
            var rounding = new TableLayoutPanel
            {
                Dock = DockStyle.Fill,
                ColumnCount = 1,
                RowCount = 2,
                AutoSize = true,
                AutoSizeMode = AutoSizeMode.GrowAndShrink,
                Margin = new Padding(0, 8, 0, 0),
            };
            rounding.Controls.Add(new Label
            {
                Text = "Rounding mode",
                AutoSize = true,
                ForeColor = SystemColors.GrayText,
                Margin = new Padding(0, 0, 0, 2),
            }, 0, 0);

            _rounding.Dock = DockStyle.Fill;
            _rounding.DropDownStyle = ComboBoxStyle.DropDownList;
            _rounding.Margin = new Padding(0);
            foreach (var rm in Formats.RoundingModes)
            {
                _rounding.Items.Add(rm);
            }

            // Show the descriptive label while keeping the plain code as the item value,
            // so SelectedItem stays the thing we pass to crackNum.
            _rounding.DrawMode = DrawMode.OwnerDrawFixed;
            _rounding.DrawItem += OnDrawRounding;
            _rounding.SelectedItem = "RNE";
            _rounding.SelectedIndexChanged += (s, e) => RunCrack();
            rounding.Controls.Add(_rounding, 0, 1);
            side.Controls.Add(rounding, 0, 1);

            // Custom parameters
            var customWrap = new TableLayoutPanel
            {
                Dock = DockStyle.Fill,
                ColumnCount = 1,
                RowCount = 2,
                AutoSize = true,
                AutoSizeMode = AutoSizeMode.GrowAndShrink,
                Margin = new Padding(0, 8, 0, 0),
            };
            _customHeading.Text = "Custom format:";
            _customHeading.AutoSize = true;
            _customHeading.ForeColor = SystemColors.GrayText;
            _customHeading.Margin = new Padding(0, 0, 0, 2);
            customWrap.Controls.Add(_customHeading, 0, 0);

            var box = new GroupBox { Dock = DockStyle.Fill, AutoSize = true, Text = string.Empty, Margin = new Padding(0) };
            var grid = new TableLayoutPanel
            {
                Dock = DockStyle.Fill,
                ColumnCount = 2,
                RowCount = 2,
                AutoSize = true,
                AutoSizeMode = AutoSizeMode.GrowAndShrink,
                Padding = new Padding(6),
            };
            grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100f));
            grid.ColumnStyles.Add(new ColumnStyle(SizeType.Absolute, 80f));

            grid.Controls.Add(_bitWidthLabel, 0, 0);
            SetUpWidthField(_bitWidth, "64");
            grid.Controls.Add(_bitWidth, 1, 0);

            grid.Controls.Add(_expWidthLabel, 0, 1);
            SetUpWidthField(_expWidth, "11");
            grid.Controls.Add(_expWidth, 1, 1);

            box.Controls.Add(grid);
            customWrap.Controls.Add(box, 0, 1);
            side.Controls.Add(customWrap, 0, 2);

            return side;
        }

        private static Label MakeFieldLabel(string text)
        {
            return new Label
            {
                Text = text,
                AutoSize = true,
                Font = new Font("Consolas", 10f),
                TextAlign = ContentAlignment.MiddleLeft,
                Anchor = AnchorStyles.Left,
                Margin = new Padding(0, 6, 3, 3),
            };
        }

        private void SetUpWidthField(TextBox box, string initial)
        {
            box.Text = initial;
            box.Dock = DockStyle.Fill;
            box.TextAlign = HorizontalAlignment.Right;
            box.Font = new Font("Consolas", 10f);
            box.KeyDown += OnEnterRun;
        }

        private Button MakeButton(string text, string tip, EventHandler onClick)
        {
            var b = new Button
            {
                Text = text,
                Width = 34,
                Height = 26,
                Margin = new Padding(0, 3, 4, 3),
                UseVisualStyleBackColor = true,
            };
            b.Click += onClick;
            _tips.SetToolTip(b, tip);
            return b;
        }

        private void OnDrawRounding(object sender, DrawItemEventArgs e)
        {
            e.DrawBackground();

            if (e.Index >= 0)
            {
                var code = (string)_rounding.Items[e.Index];
                string label;
                if (!Formats.RoundingLabels.TryGetValue(code, out label))
                {
                    label = code;
                }

                TextRenderer.DrawText(e.Graphics, label, e.Font, e.Bounds, e.ForeColor, TextFormatFlags.Left | TextFormatFlags.VerticalCenter);
            }

            e.DrawFocusRectangle();
        }

        private void OnEnterRun(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                e.SuppressKeyPress = true;   // no ding
                e.Handled = true;
                RunCrack();
            }
        }

        private void OnFormatSelected(object sender, TreeViewEventArgs e)
        {
            var fmt = e.Node?.Tag as Format;
            if (fmt == null)
            {
                // A section heading. Leave the current selection alone rather than
                // clearing the results the user is looking at.
                return;
            }

            _selection = fmt.Id;
            SyncCustomBox();
            RunCrack();
        }

        /// <summary>
        /// Point the shared width box at whatever is selected. It serves all three
        /// "Custom" entries -- IEEE-754 float, signed integer, and unsigned word --
        /// but only the float has an exponent, and with a fixed format selected
        /// nothing in the box does anything at all. So the heading and which rows
        /// stay live both follow the selection, rather than naming one of the three
        /// and hoping.
        /// </summary>
        private void SyncCustomBox()
        {
            var box = CustomBox.For(Formats.ById(_selection));

            _customHeading.Text    = box.Heading;
            _bitWidthLabel.Enabled = box.WidthApplies;
            _bitWidth.Enabled      = box.WidthApplies;
            _expWidthLabel.Enabled = box.ExponentApplies;
            _expWidth.Enabled      = box.ExponentApplies;
        }

        private void SelectFormat(string id)
        {
            foreach (TreeNode head in _formats.Nodes)
            {
                foreach (TreeNode node in head.Nodes)
                {
                    var fmt = node.Tag as Format;
                    if (fmt != null && fmt.Id == id)
                    {
                        _formats.SelectedNode = node;   // fires AfterSelect, which cracks
                        return;
                    }
                }
            }
        }

        private void Zoom(int delta)
        {
            _fontSize = Math.Max(6f, _fontSize + delta);
            _output.Font = new Font("Consolas", _fontSize);
        }

        private void RunCrack()
        {
            var fmt = Formats.ById(_selection);
            if (fmt == null)
            {
                return;
            }

            var bw = ParseWidth(_bitWidth.Text);
            var ew = ParseWidth(_expWidth.Text);

            var flag = FlagResult.For(fmt, bw, ew);
            if (!flag.IsValid)
            {
                SetOutput(flag.Invalid);
                return;
            }

            // An empty box is not a value; see ValueInput for why this is not defaulted.
            var input = ValueInput.For(_value.Text);

            string text;
            if (!input.HasValue)
            {
                text = ValueInput.Prompt;
            }
            else
            {
                var saved = Cursor.Current;
                Cursor.Current = Cursors.WaitCursor;
                try
                {
                    text = Runner.Run(flag.Flag, (string)_rounding.SelectedItem, input.Value);
                }
                finally
                {
                    Cursor.Current = saved;
                }
            }

            string kind;
            if (text.Contains("ENCODED"))
            {
                kind = "Encoding in format";
            }
            else if (text.Contains("DECODED"))
            {
                kind = "Decoded using format";
            }
            else
            {
                kind = "Format";
            }

            SetOutput("[" + kind + ": " + fmt.Label + "]\n\n" + text);
        }

        private static int ParseWidth(string s)
        {
            int n;
            return int.TryParse(s, NumberStyles.Integer, CultureInfo.InvariantCulture, out n) ? n : 0;
        }

        /// <summary>
        /// Put text in the output pane, normalizing line endings first.
        /// </summary>
        /// <remarks>
        /// crackNum emits bare LF, and a Win32 multiline edit control does not treat
        /// that as a line break -- the whole report would arrive as one long line
        /// punctuated by boxes. Everything is normalized to CRLF on the way in.
        /// </remarks>
        private void SetOutput(string text)
        {
            var normalized = (text ?? string.Empty).Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n");
            _output.Text = normalized;
            _output.SelectionStart = 0;
            _output.SelectionLength = 0;

            // Only once there is a window to scroll. The constructor sets the welcome
            // text before the handle exists, and --selftest never creates one at all.
            if (_output.IsHandleCreated)
            {
                _output.ScrollToCaret();
            }
        }
    }
}
