import SwiftUI
import AppKit

// MARK: - Tool discovery
//
// We locate `crackNum` and `z3` purely from the user's PATH — nothing is
// hardcoded. Two tiers:
//
//   1. The PATH we already inherited. When launched from a terminal, this is the
//      user's full shell PATH and already contains everything, so we're done —
//      and we never spawn a shell.
//   2. Only if a tool isn't found there (the Finder/launchd case, where the app
//      inherits a stripped-down PATH), we ask an interactive login shell for the
//      real PATH. That case has no controlling terminal, so `zsh -i` is safe.
//
// This ordering matters: `zsh -i` opens /dev/tty directly (line editor + any
// shell-integration cursor queries in ~/.zshrc), so spawning it while a
// controlling terminal is present hangs. Launching from a terminal hits tier 1
// only, so the shell is never invoked there.

enum Tools {
    static let inheritedPath = ProcessInfo.processInfo.environment["PATH"] ?? "/usr/bin:/bin"

    /// The user's full PATH per an interactive login shell. Computed lazily, and
    /// only reached when a tool is missing from `inheritedPath`.
    static let loginPath: String = {
        let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
        let proc = Process()
        proc.executableURL = URL(fileURLWithPath: shell)
        // -i -l -c: interactive login shell so ~/.zshrc (where PATH additions
        // usually live) is sourced. Sentinels guard against banner/compinit noise.
        proc.arguments = ["-ilc", "printf '<<<PATH:%s:PATH>>>' \"$PATH\""]
        let pipe = Pipe()
        proc.standardOutput = pipe
        proc.standardError = Pipe()
        proc.standardInput = FileHandle.nullDevice
        do {
            try proc.run()
            proc.waitUntilExit()
            let raw = String(data: pipe.fileHandleForReading.readDataToEndOfFile(), encoding: .utf8) ?? ""
            if let lo = raw.range(of: "<<<PATH:"),
               let hi = raw.range(of: ":PATH>>>", range: lo.upperBound..<raw.endIndex) {
                return String(raw[lo.upperBound..<hi.lowerBound])
            }
        } catch { }
        return ""
    }()

    private static func search(_ name: String, in path: String) -> String? {
        let fm = FileManager.default
        for dir in path.split(separator: ":") {
            let candidate = "\(dir)/\(name)"
            if fm.isExecutableFile(atPath: candidate) { return candidate }
        }
        return nil
    }

    /// Locate an executable: inherited PATH first, then the login-shell PATH.
    static func locate(_ name: String) -> String? {
        search(name, in: inheritedPath) ?? search(name, in: loginPath)
    }

    static let crackNum: String? = locate("crackNum")
    static let z3: String? = locate("z3")

    /// PATH to hand to the crackNum child: the dirs of the tools we resolved,
    /// followed by the inherited PATH. Avoids forcing the login-shell query.
    static var childPath: String {
        var dirs: [String] = []
        for tool in [crackNum, z3] {
            if let t = tool { dirs.append((t as NSString).deletingLastPathComponent) }
        }
        let all = dirs + inheritedPath.split(separator: ":").map(String.init)
        var seen = Set<String>()
        return all.filter { seen.insert($0).inserted && !$0.isEmpty }.joined(separator: ":")
    }
}


// MARK: - Format definitions

struct Format: Identifiable, Hashable {
    let id: String        // stable code, e.g. "fsp"
    let label: String     // shown in the list
    let kind: Kind

    enum Kind: Hashable {
        case fixedFloat(String)   // e.g. "fe4m3", "fsp"
        case customFloat
        case fixedWord(Int)       // bit count
        case customWord
        case fixedInt(Int)
        case customInt
    }
}

struct FormatSection: Identifiable {
    let id = UUID()
    let title: String
    let formats: [Format]
}

let formatSections: [FormatSection] = [
    FormatSection(title: "Float", formats: [
        Format(id: "fe4m3", label: "FP8 (E4M3)", kind: .fixedFloat("fe4m3")),
        Format(id: "fe5m2", label: "FP8 (E5M2)", kind: .fixedFloat("fe5m2")),
        Format(id: "fhp",   label: "Half",       kind: .fixedFloat("fhp")),
        Format(id: "fbp",   label: "Brain",      kind: .fixedFloat("fbp")),
        Format(id: "fsp",   label: "Single",     kind: .fixedFloat("fsp")),
        Format(id: "fdp",   label: "Double",     kind: .fixedFloat("fdp")),
        Format(id: "fcs",   label: "Custom",     kind: .customFloat),
    ]),
    FormatSection(title: "Word (Unsigned)", formats: [
        Format(id: "w8",  label: "8-bit",  kind: .fixedWord(8)),
        Format(id: "w16", label: "16-bit", kind: .fixedWord(16)),
        Format(id: "w32", label: "32-bit", kind: .fixedWord(32)),
        Format(id: "w64", label: "64-bit", kind: .fixedWord(64)),
        Format(id: "wcs", label: "Custom", kind: .customWord),
    ]),
    FormatSection(title: "Integer (Signed)", formats: [
        Format(id: "i8",  label: "8-bit",  kind: .fixedInt(8)),
        Format(id: "i16", label: "16-bit", kind: .fixedInt(16)),
        Format(id: "i32", label: "32-bit", kind: .fixedInt(32)),
        Format(id: "i64", label: "64-bit", kind: .fixedInt(64)),
        Format(id: "ics", label: "Custom", kind: .customInt),
    ]),
]

let roundingModes = ["RNE", "RNA", "RTP", "RTN", "RTZ"]
let roundingLabels: [String: String] = [
    "RNE": "RNE (Nearest, ties to even)",
    "RNA": "RNA (Nearest, ties to away)",
    "RTP": "RTP (Toward +\u{221E})",
    "RTN": "RTN (Toward -\u{221E})",
    "RTZ": "RTZ (Toward 0)",
]

// MARK: - Runner

/// Result of building a precision flag: either the flag, or a validation message to show.
enum FlagResult {
    case flag(String)
    case invalid(String)
}

/// Build the crackNum precision flag for a format, given custom widths.
func precisionFlag(for kind: Format.Kind, bitWidth: Int, expWidth: Int) -> FlagResult {
    switch kind {
    case .fixedFloat(let f): return .flag("-\(f)")
    case .customFloat:
        let sigWidth = bitWidth - expWidth - 1
        if expWidth < 2 || sigWidth < 2 {
            return .invalid("""
            Invalid custom FP format:
              Total width: \(bitWidth)
                Sign       :    1
                Exponent   : \(String(format: "%4d", expWidth))
                Significand: \(String(format: "%4d", sigWidth)) (Total = Sign + Exponent + Significand)

            Exponent and significand must be at least 2 bits each.
            """)
        }
        // crackNum's -fE+S: E exponent bits, S significand bits *including* the implied bit.
        return .flag("-f\(expWidth)+\(bitWidth - expWidth)")
    case .fixedWord(let n): return .flag("-w\(n)")
    case .customWord:       return .flag("-w\(bitWidth)")
    case .fixedInt(let n):  return .flag("-i\(n)")
    case .customInt:        return .flag("-i\(bitWidth)")
    }
}

/// Run crackNum, returning combined stdout+stderr text (mirrors the Tcl's 2>@1).
func runCrackNum(flag: String, rounding: String, value: String) -> String {
    guard let crackNum = Tools.crackNum else {
        return "crackNum: Cannot locate the 'crackNum' binary on your PATH.\n\n"
             + "Make sure it is installed and reachable (e.g. `which crackNum` works in your terminal)."
    }
    guard let z3 = Tools.z3 else {
        return "crackNum: Cannot locate the 'z3' binary on your PATH.\n\n"
             + "Make sure it is installed and reachable (e.g. `which z3` works in your terminal)."
    }

    let rmParam = "-r\(rounding)"
    let args = [flag, rmParam, "--", value]

    let proc = Process()
    proc.executableURL = URL(fileURLWithPath: crackNum)
    proc.arguments = args
    var env = ProcessInfo.processInfo.environment
    env["SBV_Z3"] = z3
    env["PATH"] = Tools.childPath
    proc.environment = env

    let pipe = Pipe()
    proc.standardOutput = pipe
    proc.standardError = pipe   // merge stderr into stdout
    proc.standardInput = FileHandle.nullDevice   // crackNum reads its value from argv, not stdin

    do {
        try proc.run()
        let data = pipe.fileHandleForReading.readDataToEndOfFile()
        proc.waitUntilExit()
        var result = String(data: data, encoding: .utf8) ?? ""
        if proc.terminationStatus != 0 {
            let cmd = "\(crackNum) \(args.joined(separator: " "))"
            result += "\n\n** Call to crackNum failed! Make sure the value makes sense for the chosen format."
            result += "\n**"
            result += "\n**   Run: \(cmd)"
            result += "\n**"
            result += "\n**   Value : \(value)"
        }
        return result
    } catch {
        return "Failed to launch crackNum: \(error.localizedDescription)"
    }
}

// MARK: - UI

/// Parsed crackNum-style command-line arguments.
struct ParsedArgs {
    var value: String?
    var formatCode: Format.Id?
    var bitWidth: Int?
    var expWidth: Int?
    var rounding: String?
}

extension Model {
    /// Parse the crackNum flags forwarded by `crackNum --gui …`: -f<fmt>, -w<N>,
    /// -i<N>, -r<mode> in their attached form (-fsp, -w32, -rRTZ), plus `--` to
    /// introduce a value that begins with '-'. crackNum's Haskell front-end has
    /// already parsed and validated these, so we only handle the canonical
    /// attached forms; any other flag (-l lanes, -d, -v, …) is ignored.
    static func parseArgs(_ args: [String]) -> ParsedArgs {
        var p = ParsedArgs()
        var values: [String] = []
        var i = 0

        while i < args.count {
            let a = args[i]
            if a == "--" {                       // rest are values (e.g. a negative number)
                values.append(contentsOf: args[(i + 1)...])
                break
            }
            if a.hasPrefix("-f") {
                applyFloat(&p, String(a.dropFirst(2)).lowercased())
            } else if a.hasPrefix("-w") {
                applyInteger(&p, unsigned: true, String(a.dropFirst(2)))
            } else if a.hasPrefix("-i") {
                applyInteger(&p, unsigned: false, String(a.dropFirst(2)))
            } else if a.hasPrefix("-r") {
                let rm = String(a.dropFirst(2)).uppercased()
                if roundingModes.contains(rm) { p.rounding = rm }
            } else if a.hasPrefix("-") && a != "-" {
                // Some other flag (-l lanes, -d, --debug, -v, …): ignore.
            } else {
                values.append(a)
            }
            i += 1
        }

        if !values.isEmpty { p.value = values.joined(separator: " ") }
        return p
    }

    private static func applyFloat(_ p: inout ParsedArgs, _ v: String) {
        switch v {
        case "sp":   p.formatCode = "fsp"
        case "dp":   p.formatCode = "fdp"
        case "hp":   p.formatCode = "fhp"
        case "bp":   p.formatCode = "fbp"
        case "e4m3": p.formatCode = "fe4m3"
        case "e5m2": p.formatCode = "fe5m2"
        default:
            // Custom "E+S": E exponent bits, S significand bits (incl. implicit).
            let parts = v.split(separator: "+", maxSplits: 1)
            if parts.count == 2, let e = Int(parts[0]), let s = Int(parts[1]) {
                p.formatCode = "fcs"
                p.expWidth   = e
                p.bitWidth   = e + s      // total width = 1 sign + E + (S-1) stored = E + S
            }
        }
    }

    private static func applyInteger(_ p: inout ParsedArgs, unsigned: Bool, _ v: String) {
        let prefix = unsigned ? "w" : "i"
        if ["8", "16", "32", "64"].contains(v) {
            p.formatCode = prefix + v
        } else if let n = Int(v), n > 0 {
            p.formatCode = prefix + "cs"      // wcs / ics (custom width)
            p.bitWidth   = n
        }
    }
}

final class Model: ObservableObject {
    @Published var selection: Format.Id? = nil
    @Published var rounding = "RNE"
    @Published var value = ""
    @Published var bitWidth = "64"
    @Published var expWidth = "11"
    @Published var output = Model.welcome

    init() {
        // Parse crackNum-style arguments forwarded by `crackNum --gui ...`
        // (also works when running the binary directly, e.g. `CrackNum -fsp 2.5`).
        let parsed = Self.parseArgs(Array(CommandLine.arguments.dropFirst()))
        if let v  = parsed.value    { value    = v }
        if let bw = parsed.bitWidth { bitWidth = String(bw) }
        if let ew = parsed.expWidth { expWidth = String(ew) }
        if let rm = parsed.rounding { rounding = rm }
        selection = parsed.formatCode
        // If a format was supplied, crack immediately so the window opens with results.
        if parsed.formatCode != nil { run() }
    }

    static let welcome = """
    Enter a value above, then pick a format on the left to crack it.

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
      - Verilog input longer than the format is decoded as SIMD lanes.
    """

    var selectedFormat: Format? {
        guard let sel = selection else { return nil }
        return formatSections.flatMap(\.formats).first { $0.id == sel }
    }

    func run() {
        guard let fmt = selectedFormat else { return }
        let bw = Int(bitWidth) ?? 0
        let ew = Int(expWidth) ?? 0
        switch precisionFlag(for: fmt.kind, bitWidth: bw, expWidth: ew) {
        case .invalid(let msg):
            output = msg
        case .flag(let flag):
            let val = value.isEmpty ? "0" : value
            let text = runCrackNum(flag: flag, rounding: rounding, value: val)
            let kind: String
            if text.contains("ENCODED") { kind = "Encoding in format" }
            else if text.contains("DECODED") { kind = "Decoded using format" }
            else { kind = "Format" }
            output = "[\(kind): \(fmt.label)]\n\n\(text)"
        }
    }
}

extension Format { typealias Id = String }

struct ContentView: View {
    @StateObject private var model = Model()
    @State private var fontSize: CGFloat = 12

    var body: some View {
        VStack(spacing: 8) {
            // Value entry row (with zoom / help controls on the left)
            HStack {
                Button { fontSize = max(6, fontSize - 1) } label: { Image(systemName: "minus.magnifyingglass") }
                Button { fontSize += 1 } label: { Image(systemName: "plus.magnifyingglass") }
                Button { model.output = Model.welcome } label: { Image(systemName: "questionmark.circle") }
                Spacer()
                Text("Value")
                    .font(.system(.body, design: .monospaced))
                TextField("Enter a number or bit pattern", text: $model.value)
                    .textFieldStyle(.roundedBorder)
                    .font(.system(.body, design: .monospaced))
                    .frame(width: 340)
                    .onSubmit { model.run() }
            }
            .padding(.horizontal)
            .padding(.top, 8)

            HStack(alignment: .top, spacing: 8) {
                sidebar
                    .frame(width: 240)

                // Results
                GeometryReader { geo in
                    ScrollView([.vertical, .horizontal]) {
                        Text(model.output)
                            .font(.system(size: fontSize, design: .monospaced))
                            .textSelection(.enabled)
                            .multilineTextAlignment(.leading)
                            .padding(8)
                            // Fill the viewport and pin content to the top-left,
                            // so short output starts at the top and longer output scrolls.
                            .frame(minWidth: geo.size.width,
                                   minHeight: geo.size.height,
                                   alignment: .topLeading)
                    }
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .background(Color(nsColor: .textBackgroundColor))
                .border(Color(nsColor: .separatorColor))
            }
            .padding(.horizontal)
            .padding(.bottom)
        }
        .frame(minWidth: 820, minHeight: 560)
    }

    private var sidebar: some View {
        VStack(alignment: .leading, spacing: 8) {
            List(selection: $model.selection) {
                ForEach(formatSections) { section in
                    Section(section.title) {
                        ForEach(section.formats) { fmt in
                            Text(fmt.label).tag(fmt.id)
                        }
                    }
                }
            }
            .frame(minHeight: 260)
            .onChange(of: model.selection) { _ in model.run() }

            // Rounding mode
            VStack(alignment: .leading, spacing: 2) {
                Text("Rounding mode").font(.caption).foregroundStyle(.secondary)
                Picker("", selection: $model.rounding) {
                    ForEach(roundingModes, id: \.self) { rm in
                        Text(roundingLabels[rm] ?? rm).tag(rm)
                    }
                }
                .labelsHidden()
                .onChange(of: model.rounding) { _ in model.run() }
            }

            // Custom parameters
            GroupBox("Custom parameters") {
                VStack(alignment: .leading, spacing: 6) {
                    HStack {
                        Text("Total width:")
                        Spacer()
                        TextField("", text: $model.bitWidth)
                            .frame(width: 70)
                            .multilineTextAlignment(.trailing)
                            .onSubmit { model.run() }
                    }
                    HStack {
                        Text("Exponent width:")
                        Spacer()
                        TextField("", text: $model.expWidth)
                            .frame(width: 70)
                            .multilineTextAlignment(.trailing)
                            .onSubmit { model.run() }
                    }
                    Text("(exponent width applies to custom floats)")
                        .font(.caption2).foregroundStyle(.secondary)
                }
                .font(.system(.body, design: .monospaced))
            }
        }
    }
}

// MARK: - App entry
//
// We drive NSApplication directly and create the window in AppKit (hosting the
// SwiftUI view) rather than using SwiftUI's `WindowGroup` scene. SwiftUI's scene
// lifecycle only creates its window reliably for a proper .app bundle; a bare
// executable run from a terminal ends up with no window at all. Creating the
// NSWindow ourselves works identically whether bundled or not.

final class AppDelegate: NSObject, NSApplicationDelegate {
    var window: NSWindow?

    func applicationDidFinishLaunching(_ notification: Notification) {
        NSApp.setActivationPolicy(.regular)

        let hosting = NSHostingController(rootView: ContentView())
        let window = NSWindow(contentViewController: hosting)
        window.title = "CrackNum"
        window.styleMask = [.titled, .closable, .miniaturizable, .resizable]
        window.setContentSize(NSSize(width: 1040, height: 600))
        window.center()
        self.window = window   // retain it

        window.makeKeyAndOrderFront(nil)
        window.orderFrontRegardless()
        NSApp.activate(ignoringOtherApps: true)
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

/// Build a minimal main menu so ⌘Q / ⌘W / ⌘C-V etc. work.
private func makeMainMenu() -> NSMenu {
    let mainMenu = NSMenu()

    let appItem = NSMenuItem()
    mainMenu.addItem(appItem)
    let appMenu = NSMenu()
    appItem.submenu = appMenu
    appMenu.addItem(withTitle: "Hide CrackNum", action: #selector(NSApplication.hide(_:)), keyEquivalent: "h")
    appMenu.addItem(.separator())
    appMenu.addItem(withTitle: "Quit CrackNum", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")

    let editItem = NSMenuItem()
    mainMenu.addItem(editItem)
    let editMenu = NSMenu(title: "Edit")
    editItem.submenu = editMenu
    editMenu.addItem(withTitle: "Cut",        action: #selector(NSText.cut(_:)),       keyEquivalent: "x")
    editMenu.addItem(withTitle: "Copy",       action: #selector(NSText.copy(_:)),      keyEquivalent: "c")
    editMenu.addItem(withTitle: "Paste",      action: #selector(NSText.paste(_:)),     keyEquivalent: "v")
    editMenu.addItem(withTitle: "Select All", action: #selector(NSText.selectAll(_:)), keyEquivalent: "a")

    return mainMenu
}

@main
enum Main {
    static func main() {
        let app = NSApplication.shared
        app.mainMenu = makeMainMenu()
        let delegate = AppDelegate()
        app.delegate = delegate
        app.run()
    }
}
