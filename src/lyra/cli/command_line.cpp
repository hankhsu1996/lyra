#include "lyra/cli/command_line.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <expected>
#include <filesystem>
#include <format>
#include <initializer_list>
#include <iterator>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <thread>
#include <unistd.h>
#include <utility>
#include <vector>

#include <fmt/core.h>
#include <slang/driver/Driver.h>
#include <slang/util/CommandLine.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/cli/manifest.hpp"
#include "lyra/driver/artifact_store.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/status/status.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace lyra::cli {

namespace {

// Each option Lyra adds to the command line, as one thing a command either
// acts on or does not. The simulation's own arguments count as one: a command
// that runs no simulation has nobody to hand them to.
enum class LyraOption : std::uint8_t {
  kColor,
  kNoColor,
  kFormat,
  kAssertions,
  kRemarks,
  kConfig,
  kRelease,
  kNoPch,
  kCacheDir,
  kRebuild,
  kCxx,
  kJobs,
  kOut,
  kBackend,
  kDpiLink,
  kTimeTrace,
  kTimeTraceGranularity,
  kStatsFile,
  kProgress,
  kSimulationArgs,
};

constexpr std::size_t kLyraOptionCount =
    static_cast<std::size_t>(LyraOption::kSimulationArgs) + 1;

// Which options a command acts on.
class OptionSet {
 public:
  constexpr OptionSet(std::initializer_list<LyraOption> options) {
    for (const LyraOption option : options) {
      bits_ |= Bit(option);
    }
  }

  [[nodiscard]] constexpr auto operator|(OptionSet other) const -> OptionSet {
    OptionSet joined = *this;
    joined.bits_ |= other.bits_;
    return joined;
  }

  [[nodiscard]] constexpr auto Contains(LyraOption option) const -> bool {
    return (bits_ & Bit(option)) != 0;
  }

 private:
  static constexpr auto Bit(LyraOption option) -> std::uint32_t {
    return std::uint32_t{1} << static_cast<unsigned>(option);
  }

  std::uint32_t bits_ = 0;
};

// What every command reading a design acts on: how its report is coloured,
// where the design's declaration is, whether the run reports on itself, and
// how it shows what it is doing meanwhile.
constexpr OptionSet kReadsADesign = {
    LyraOption::kColor,
    LyraOption::kNoColor,
    LyraOption::kConfig,
    LyraOption::kTimeTrace,
    LyraOption::kTimeTraceGranularity,
    LyraOption::kStatsFile,
    LyraOption::kProgress};
// What every command lowering the design acts on besides: the policy lowering
// follows, and whether what it could have done better reaches the report.
constexpr OptionSet kLowersADesign =
    kReadsADesign | OptionSet{LyraOption::kAssertions, LyraOption::kRemarks};
// What building the design's program acts on besides.
constexpr OptionSet kBuildsAProgram =
    kLowersADesign | OptionSet{LyraOption::kRelease,  LyraOption::kNoPch,
                               LyraOption::kCacheDir, LyraOption::kRebuild,
                               LyraOption::kCxx,      LyraOption::kJobs,
                               LyraOption::kBackend,  LyraOption::kDpiLink};

// Where a command shows what it is doing when nobody said. The error stream
// carries it, so the answer follows from what else the command's streams are
// for: one whose product is what it prints shows nothing, one that runs the
// design leaves a stream that is not a terminal to the program, and the rest
// show it wherever that stream goes.
enum class SaysStatus : std::uint8_t { kNever, kOnATerminal, kAlways };

// Every command in one place: how it is spelled, and the facts a command
// decides rather than inherits -- whether it needs somewhere to write, where it
// says how far it has got, and which options it acts on. A command is named by
// a verb and an object, with an empty object for a verb that stands alone. The
// usage text, the parse, the output check, and the check that refuses an option
// a command does not act on all read this table, so adding a command is one row
// that has to say all of it, rather than several lists that drift apart.
struct CommandSpec {
  std::string_view verb;
  std::string_view object;
  CommandKind kind;
  bool requires_out;
  SaysStatus status;
  OptionSet takes;
};

// Entries sharing a verb stay adjacent: the usage line groups them by that
// adjacency into `dump hir|mir|lir|llvm`.
constexpr auto kCommands = std::to_array<CommandSpec>(
    {{.verb = "check",
      .object = "",
      .kind = CommandKind::kCheck,
      .requires_out = false,
      .status = SaysStatus::kAlways,
      .takes = kReadsADesign},
     {.verb = "dump",
      .object = "ast",
      .kind = CommandKind::kDumpAst,
      .requires_out = false,
      .status = SaysStatus::kNever,
      .takes = kReadsADesign},
     {.verb = "dump",
      .object = "hir",
      .kind = CommandKind::kDumpHir,
      .requires_out = false,
      .status = SaysStatus::kNever,
      .takes = kLowersADesign},
     {.verb = "dump",
      .object = "mir",
      .kind = CommandKind::kDumpMir,
      .requires_out = false,
      .status = SaysStatus::kNever,
      .takes = kLowersADesign},
     {.verb = "dump",
      .object = "lir",
      .kind = CommandKind::kDumpLir,
      .requires_out = false,
      .status = SaysStatus::kNever,
      .takes = kLowersADesign},
     {.verb = "dump",
      .object = "llvm",
      .kind = CommandKind::kDumpLlvm,
      .requires_out = false,
      .status = SaysStatus::kNever,
      .takes = kLowersADesign},
     // The project's recipe is written with the compiler and the level it
     // compiles at baked in, and takes its own width and precompiled header
     // when it is run.
     {.verb = "emit",
      .object = "cpp",
      .kind = CommandKind::kEmitCpp,
      .requires_out = true,
      .status = SaysStatus::kAlways,
      .takes = kLowersADesign |
               OptionSet{
                   LyraOption::kFormat, LyraOption::kRelease, LyraOption::kCxx,
                   LyraOption::kOut, LyraOption::kDpiLink}},
     {.verb = "build",
      .object = "",
      .kind = CommandKind::kBuild,
      .requires_out = false,
      .status = SaysStatus::kAlways,
      .takes = kBuildsAProgram | OptionSet{LyraOption::kOut}},
     {.verb = "run",
      .object = "",
      .kind = CommandKind::kRun,
      .requires_out = false,
      .status = SaysStatus::kOnATerminal,
      .takes = kBuildsAProgram | OptionSet{LyraOption::kSimulationArgs}},
     {.verb = "cache",
      .object = "clear",
      .kind = CommandKind::kCacheClear,
      .requires_out = false,
      .status = SaysStatus::kNever,
      .takes = {
          LyraOption::kColor, LyraOption::kNoColor, LyraOption::kCacheDir}}});

// How an option is spelled on the command line.
auto Spelling(LyraOption option) -> std::string_view {
  switch (option) {
    case LyraOption::kColor:
      return "--color";
    case LyraOption::kNoColor:
      return "--no-color";
    case LyraOption::kFormat:
      return "--format";
    case LyraOption::kAssertions:
      return "--assertions";
    case LyraOption::kRemarks:
      return "--remarks";
    case LyraOption::kConfig:
      return "--config";
    case LyraOption::kRelease:
      return "--release";
    case LyraOption::kNoPch:
      return "--no-pch";
    case LyraOption::kCacheDir:
      return "--cache-dir";
    case LyraOption::kRebuild:
      return "--rebuild";
    case LyraOption::kCxx:
      return "--cxx";
    case LyraOption::kJobs:
      return "--jobs";
    case LyraOption::kOut:
      return "--out";
    case LyraOption::kBackend:
      return "--backend";
    case LyraOption::kDpiLink:
      return "--dpi-link";
    case LyraOption::kTimeTrace:
      return "--time-trace";
    case LyraOption::kTimeTraceGranularity:
      return "--time-trace-granularity";
    case LyraOption::kStatsFile:
      return "--stats-file";
    case LyraOption::kProgress:
      return "--progress";
    case LyraOption::kSimulationArgs:
      return "arguments after `--`";
  }
  throw InternalError("an option has no spelling");
}

// Whether the caller gave the option.
auto IsGiven(
    LyraOption option, const CliOptions& opts, bool has_simulation_args)
    -> bool {
  switch (option) {
    case LyraOption::kColor:
      return opts.color.has_value();
    case LyraOption::kNoColor:
      return opts.no_color.has_value();
    case LyraOption::kFormat:
      return opts.format.has_value();
    case LyraOption::kAssertions:
      return opts.assertions.has_value();
    case LyraOption::kRemarks:
      return opts.remarks.has_value();
    case LyraOption::kConfig:
      return opts.config.has_value();
    case LyraOption::kRelease:
      return opts.release.has_value();
    case LyraOption::kNoPch:
      return opts.no_pch.has_value();
    case LyraOption::kCacheDir:
      return opts.cache_dir.has_value();
    case LyraOption::kRebuild:
      return opts.rebuild.has_value();
    case LyraOption::kCxx:
      return opts.cxx.has_value();
    case LyraOption::kJobs:
      return opts.jobs.has_value();
    case LyraOption::kOut:
      return opts.out.has_value();
    case LyraOption::kBackend:
      return opts.backend.has_value();
    case LyraOption::kDpiLink:
      return !opts.dpi_link.empty();
    case LyraOption::kTimeTrace:
      return opts.time_trace.has_value();
    case LyraOption::kTimeTraceGranularity:
      return opts.time_trace_granularity.has_value();
    case LyraOption::kStatsFile:
      return opts.stats_file.has_value();
    case LyraOption::kProgress:
      return opts.progress.has_value();
    case LyraOption::kSimulationArgs:
      return has_simulation_args;
  }
  throw InternalError("an option has no reading");
}

// The spelling of each backend on the command line. Returns nullopt
// for a name outside the table, which is a user typing a value the command
// line cannot restrict rather than a drift between two internal lists.
auto ParseBackend(std::string_view name) -> std::optional<Backend> {
  static constexpr std::array<std::pair<std::string_view, Backend>, 2> kNames =
      {{{"cpp", Backend::kCpp}, {"llvm", Backend::kLlvm}}};
  const auto* const it =
      std::ranges::find(kNames, name, &decltype(kNames)::value_type::first);
  if (it == kNames.end()) {
    return std::nullopt;
  }
  return it->second;
}

// The spelling of each assertion policy, in the same shape as the backend's: a
// name outside the table is a caller typing a value the command line cannot
// restrict rather than a drift between two internal lists.
auto ParseAssertionPolicy(std::string_view name)
    -> std::optional<support::AssertionPolicy> {
  static constexpr std::array<
      std::pair<std::string_view, support::AssertionPolicy>, 2>
      kNames = {
          {{"check", support::AssertionPolicy::kCheck},
           {"skip", support::AssertionPolicy::kSkip}}};
  const auto* const it =
      std::ranges::find(kNames, name, &decltype(kNames)::value_type::first);
  if (it == kNames.end()) {
    return std::nullopt;
  }
  return it->second;
}

// What `--progress` asks for: the display the command would choose for itself,
// lines whatever the stream is, or nothing.
enum class Progress : std::uint8_t { kAuto, kPlain, kNone };

auto ParseProgress(std::string_view name) -> std::optional<Progress> {
  static constexpr std::array<std::pair<std::string_view, Progress>, 3> kNames =
      {{{"auto", Progress::kAuto},
        {"plain", Progress::kPlain},
        {"none", Progress::kNone}}};
  const auto* const it =
      std::ranges::find(kNames, name, &decltype(kNames)::value_type::first);
  if (it == kNames.end()) {
    return std::nullopt;
  }
  return it->second;
}

// Whether the terminal shows how far a command has got on its own window when
// told. The sequence that tells it means something else to some terminals that
// were not written for it, so it is sent only to the ones known to take it,
// each recognized by what it sets in the environment of what it runs.
auto TerminalShowsProgress() -> bool {
  const auto set_to = [](const char* variable) -> std::string_view {
    const char* const value = std::getenv(variable);
    return value == nullptr ? std::string_view{} : std::string_view(value);
  };
  const std::string_view program = set_to("TERM_PROGRAM");
  return !set_to("WT_SESSION").empty() || set_to("ConEmuANSI") == "ON" ||
         program == "WezTerm" || program == "ghostty";
}

// Whether diagnostics carry ANSI colour. `kAuto` asks the terminal; the other
// two are the caller overriding that answer in either direction.
enum class ColorPreference : std::uint8_t { kAuto, kAlways, kNever };

auto ColorPreferenceOf(const CliOptions& opts) -> ColorPreference {
  if (opts.no_color.value_or(false)) {
    return ColorPreference::kNever;
  }
  if (opts.color.value_or(false)) {
    return ColorPreference::kAlways;
  }
  return ColorPreference::kAuto;
}

auto FindCommand(CommandKind cmd) -> const CommandSpec& {
  const auto* const it = std::ranges::find(kCommands, cmd, &CommandSpec::kind);
  if (it == kCommands.end()) {
    throw InternalError("command kind is absent from the command table");
  }
  return *it;
}

auto CommandSpelling(CommandKind cmd) -> std::string {
  const auto& spec = FindCommand(cmd);
  return spec.object.empty() ? std::string(spec.verb)
                             : std::format("{} {}", spec.verb, spec.object);
}

// Which commands write their output somewhere the caller has to name.
auto RequiresOut(CommandKind cmd) -> bool {
  return FindCommand(cmd).requires_out;
}

auto CommandList() -> std::string {
  std::string commands;
  std::string_view grouped_verb;
  for (const auto& spec : kCommands) {
    if (spec.verb == grouped_verb) {
      commands += std::format("|{}", spec.object);
      continue;
    }
    if (!commands.empty()) {
      commands += ", ";
    }
    commands += CommandSpelling(spec.kind);
    grouped_verb = spec.verb;
  }
  return commands;
}

auto Usage() -> std::string {
  return std::format(
      "usage: lyra <command> [options] [files...] [-- <program args>]\n"
      "commands: {}\n",
      CommandList());
}

// The objects a verb accepts, spelled as prose for the message a caller reads
// when they named the verb but not the object.
auto ObjectChoices(std::string_view verb) -> std::string {
  std::vector<std::string> quoted;
  for (const auto& spec : kCommands) {
    if (spec.verb == verb && !spec.object.empty()) {
      quoted.push_back(std::format("'{}'", spec.object));
    }
  }
  std::string out;
  for (std::size_t i = 0; i < quoted.size(); ++i) {
    if (i > 0) {
      out += quoted.size() > 2 ? ", " : " ";
      if (i + 1 == quoted.size()) {
        out += "or ";
      }
    }
    out += quoted[i];
  }
  return out;
}

// Reads the leading words that name the command. They are positional and
// consumed here rather than registered as options, because a command chooses
// which options even apply.
auto ParseCommand(std::span<char* const> words)
    -> std::expected<std::pair<CommandKind, std::size_t>, std::string> {
  const auto word = [&](std::size_t i) -> std::string_view {
    return i < words.size() ? std::string_view(words[i]) : std::string_view{};
  };
  const auto verb = word(0);
  const auto object = word(1);

  bool verb_exists = false;
  for (const auto& spec : kCommands) {
    if (spec.verb != verb) {
      continue;
    }
    verb_exists = true;
    if (spec.object.empty()) return std::pair{spec.kind, 1UZ};
    if (spec.object == object) return std::pair{spec.kind, 2UZ};
  }
  if (verb_exists) {
    return std::unexpected(
        std::format("{} requires {}", verb, ObjectChoices(verb)));
  }
  return std::unexpected(Usage());
}

}  // namespace

void RegisterCliOptions(slang::CommandLine& cmd, CliOptions& opts) {
  cmd.add(
      "--color", opts.color,
      "force ANSI color in diagnostics, overriding TTY detection");
  cmd.add("--no-color", opts.no_color, "disable ANSI color in diagnostics");
  cmd.add(
      "--format", opts.format,
      "reformat the emitted C++ with clang-format (skipped if absent)");
  cmd.add(
      "--assertions", opts.assertions,
      "hold the design to its assertions, or elide them during lowering",
      "check|skip");
  cmd.add(
      "--remarks", opts.remarks,
      "report what the compiler could have done better and did not; the "
      "program is right either way");
  cmd.add(
      "--config", opts.config,
      "read this lyra.toml instead of searching for one", "<file>",
      slang::CommandLineFlags::FilePath);
  cmd.add(
      "--release", opts.release,
      "optimize the simulation rather than the time to build it");
  cmd.add(
      "--no-pch", opts.no_pch,
      "compile the C++ backend's output without a precompiled header");
  cmd.add(
      "--cache-dir", opts.cache_dir,
      "where built programs, compiled units and prepared headers are kept for "
      "reuse",
      "<dir>", slang::CommandLineFlags::FilePath);
  cmd.add(
      "--rebuild", opts.rebuild,
      "build as though nothing were kept, and keep what is built");
  cmd.add(
      "--cxx", opts.cxx,
      "host C++ compiler a program is compiled and linked with: a path, or a "
      "name found on PATH; clang++, then c++, when not given",
      "<program>");
  cmd.add(
      "-j,--jobs", opts.jobs,
      "how many of the design's units to lower and compile at once; "
      "0 asks for one per processor",
      "<count>");
  cmd.add(
      "-o,--out", opts.out,
      "where to write the output: the program for `build`, the project "
      "directory for `emit cpp`",
      "<path>", slang::CommandLineFlags::FilePath);
  cmd.add(
      "--backend", opts.backend,
      "which backend turns the design into a program", "cpp|llvm");
  cmd.add(
      "--dpi-link", opts.dpi_link,
      "native source (.c/.cpp) providing DPI-C foreign symbols to link",
      "<file>", slang::CommandLineFlags::FilePath);
  cmd.add(
      "--time-trace", opts.time_trace,
      "write where this run's time went, per stage, unit, scope and function, "
      "as a Chrome trace",
      "<file>", slang::CommandLineFlags::FilePath);
  cmd.add(
      "--time-trace-granularity", opts.time_trace_granularity,
      "leave out of the time trace any span shorter than this many "
      "microseconds; 500 when not given",
      "<us>");
  cmd.add(
      "--stats-file", opts.stats_file,
      "write this run's numbers as JSON: each stage's peak memory, what each "
      "unit wrote, and how long each tool the build ran took",
      "<file>", slang::CommandLineFlags::FilePath);
  cmd.add(
      "--progress", opts.progress,
      "how the command shows what it is doing: redrawn in place on a "
      "terminal and a line at intervals elsewhere (auto), lines wherever it "
      "is written (plain), or nothing (none)",
      "auto|plain|none");
}

auto SplitAtSeparator(std::span<char* const> raw) -> SplitArgv {
  const auto separator = std::ranges::find_if(
      raw, [](const char* word) { return std::string_view(word) == "--"; });
  SplitArgv out;
  out.lyra.assign(raw.begin(), separator);
  for (const auto* const word : std::ranges::subrange(
           separator == raw.end() ? separator : std::next(separator),
           raw.end())) {
    out.child.emplace_back(word);
  }
  return out;
}

auto WantsHelp(std::span<char* const> words) -> bool {
  return std::ranges::any_of(words, [](const char* raw) {
    const auto word = std::string_view(raw);
    return word == "-h" || word == "--help";
  });
}

void PrintHelp(slang::driver::Driver& driver) {
  // The usage line is rendered from the program name, so naming the program
  // "lyra <command>" is what puts the command word where a reader expects it.
  driver.cmdLine.setProgramName("lyra <command>");
  fmt::print(
      "{}", driver.cmdLine.getHelpText(
                std::format(
                    "lyra -- a SystemVerilog simulator\n\ncommands: {}\n\n"
                    "Everything after a standalone `--` is the simulation's "
                    "own argv, "
                    "which is where\nplusargs go.",
                    CommandList())));
}

auto ParseCommandWords(slang::driver::Driver& driver, std::vector<char*>& words)
    -> std::expected<CommandKind, std::string> {
  auto command = ParseCommand(
      std::span<char* const>(words).subspan(words.empty() ? 0 : 1));
  if (!command) {
    return std::unexpected(command.error());
  }
  // What the parser sees is the program name followed by options and sources.
  words.erase(
      std::next(words.begin()),
      std::next(
          words.begin(), 1 + static_cast<std::ptrdiff_t>(command->second)));
  if (!driver.parseCommandLine(static_cast<int>(words.size()), words.data())) {
    return std::unexpected("");
  }
  return command->first;
}

void PredefineToolIdentity(slang::driver::Driver& driver) {
  driver.options.defines.emplace(driver.options.defines.begin(), "__lyra__=1");
}

auto RefuseOptionsNotTaken(
    const CliOptions& opts, CommandKind cmd, bool has_simulation_args)
    -> std::expected<void, std::string> {
  std::string refused;
  for (std::size_t i = 0; i < kLyraOptionCount; ++i) {
    const auto option = static_cast<LyraOption>(i);
    if (!IsGiven(option, opts, has_simulation_args) ||
        FindCommand(cmd).takes.Contains(option)) {
      continue;
    }
    std::string takers;
    for (const CommandSpec& spec : kCommands) {
      if (spec.takes.Contains(option)) {
        takers += std::format(
            "{}`{}`", takers.empty() ? "" : ", ", CommandSpelling(spec.kind));
      }
    }
    refused += std::format(
        "{}{} means nothing to `{}`; it is taken by {}",
        refused.empty() ? "" : "\n", Spelling(option), CommandSpelling(cmd),
        takers);
  }
  if (refused.empty()) {
    return {};
  }
  return std::unexpected(std::move(refused));
}

auto UseColor(const CliOptions& opts) -> bool {
  switch (ColorPreferenceOf(opts)) {
    case ColorPreference::kNever:
      return false;
    case ColorPreference::kAlways:
      return true;
    case ColorPreference::kAuto:
      return ::isatty(STDERR_FILENO) != 0;
  }
  return false;
}

namespace {

// The display a command chooses when nobody said.
auto OwnDisplayOf(CommandKind cmd) -> status::Display {
  // A terminal that says it is dumb takes no instruction to redraw a line.
  const char* const terminal = std::getenv("TERM");
  const bool redraws = ::isatty(STDERR_FILENO) != 0 && terminal != nullptr &&
                       std::string_view(terminal) != "dumb";
  switch (FindCommand(cmd).status) {
    case SaysStatus::kNever:
      return status::Display::kNone;
    case SaysStatus::kOnATerminal:
      return redraws ? status::Display::kInPlace : status::Display::kNone;
    case SaysStatus::kAlways:
      return redraws ? status::Display::kInPlace
                     : status::Display::kPeriodicLines;
  }
  throw InternalError("a command does not say where it shows its status");
}

auto DisplayOf(CommandKind cmd, Progress asked) -> status::Display {
  switch (asked) {
    case Progress::kAuto:
      return OwnDisplayOf(cmd);
    case Progress::kPlain:
      return status::Display::kPeriodicLines;
    case Progress::kNone:
      return status::Display::kNone;
  }
  throw InternalError("a request for progress names no display");
}

}  // namespace

auto StatusLookOf(CommandKind cmd, const CliOptions& opts, bool use_color)
    -> std::expected<status::Look, std::string> {
  const auto asked = ParseProgress(opts.progress.value_or("auto"));
  if (!asked) {
    return std::unexpected(
        std::format(
            "--progress: '{}' is not one of auto, plain, none",
            *opts.progress));
  }
  return status::Look{
      .display = DisplayOf(cmd, *asked),
      .color = use_color,
      .tells_the_terminal = TerminalShowsProgress()};
}

// How many host compiles this invocation may run at once, resolved to a
// positive count here so no later step has to read a zero as a request.
//
// One unless asked otherwise. How much of a machine to take is a claim about
// what else is running on it, and an invocation that was told nothing has no
// basis for one -- a conformance run drives sixteen of these builds at a time,
// and a width each of them picked for itself would multiply rather than add.
// Zero is how the caller says "this machine is mine", which is the claim a
// person at a terminal is making and a wrapping tool is not.
auto ResolveCompileWidth(std::optional<std::int32_t> jobs)
    -> std::expected<std::size_t, std::string> {
  if (!jobs.has_value()) {
    return 1;
  }
  if (*jobs < 0) {
    return std::unexpected(std::format("-j: '{}' is not a count", *jobs));
  }
  if (*jobs > 0) {
    return static_cast<std::size_t>(*jobs);
  }
  const unsigned detected = std::thread::hardware_concurrency();
  return detected == 0 ? 1 : static_cast<std::size_t>(detected);
}

auto ResolveDesignDeclaration(
    const CliOptions& opts, const slang::driver::Driver& driver)
    -> diag::Result<DesignDeclaration> {
  const auto load =
      [](const std::filesystem::path& path) -> diag::Result<DesignDeclaration> {
    auto loaded = LoadDeclarations(path);
    if (!loaded) {
      return std::unexpected(std::move(loaded.error()));
    }
    return DesignDeclaration{*std::move(loaded)};
  };

  if (opts.config) {
    return load(*opts.config);
  }
  if (driver.sourceLoader.hasFiles()) {
    return DesignDeclaration{NoSearchNeeded{}};
  }
  std::error_code ec;
  const std::filesystem::path here = std::filesystem::current_path(ec);
  if (ec) {
    return DesignDeclaration{NoSearchNeeded{}};
  }
  auto search = FindManifest(here);
  if (const auto* absent = std::get_if<ManifestAbsent>(&search)) {
    return DesignDeclaration{*absent};
  }
  return load(std::get<ManifestFound>(search).path);
}

namespace {

// A directory every unit of the build searches, after the ones given to that
// unit alone and after any added before it: the first directory holding a file
// wins.
auto AddIncludeDirectories(
    const Manifest& manifest, std::span<const std::string> dirs,
    slang::driver::Driver& driver) -> diag::Result<void> {
  for (const auto& dir : dirs) {
    if (const std::error_code ec =
            driver.sourceManager.addUserDirectories(dir)) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidManifest,
          std::format(
              "{}: include directory '{}': {}", manifest.path.string(), dir,
              ec.message()));
    }
  }
  return {};
}

// What a cell of this library may name: its own library's cells first, then
// those of each library it declared, in the order it wrote them.
auto LibraryListOf(const Manifest& manifest, std::string own_name)
    -> std::vector<std::string> {
  std::vector<std::string> list = {std::move(own_name)};
  for (const DeclaredDependency& dependency : manifest.dependencies) {
    list.push_back(dependency.name);
  }
  return list;
}

// What every library of the build is read under ahead of what it declared for
// itself -- the macro naming the tool, then what the command line said about
// reading source: the front end keeps the first definition it is given of a
// macro, and an include is taken from the first directory holding it.
struct InvocationMaterial {
  std::vector<std::string> defines;
  std::vector<std::string> undefines;
  std::vector<std::string> incdir;
};

// Where one library of a build stands in it.
struct LibraryInBuild {
  const Manifest* declaration;
  // The sources read for it, the more specific after what they stand on: the
  // library's alone where something depends on it, and the design's after
  // them where it is the one being built.
  std::vector<const SourceSet*> sets;
  // What the front end calls its library; empty for the build's default one.
  std::string front_end_library;
  bool single_unit;
};

// Reads one library in compilation units of its own, one for all its files or
// one per file (LRM 3.12.1), each under what the command line said and what
// the library declared for itself and nothing else of the build's. What it
// offers its dependents is searched by every unit of the build besides, after
// what each was given for itself.
auto AddLibrary(
    const LibraryInBuild& library, const InvocationMaterial& invocation,
    slang::driver::Driver& driver) -> diag::Result<void> {
  const Manifest& manifest = *library.declaration;
  slang::driver::SeparateUnitOptions how{
      .includePaths = invocation.incdir,
      .defines = invocation.defines,
      .undefines = invocation.undefines,
      .libraryName = library.front_end_library,
      .warningOptions = {},
      .searchDirectories = {},
      .searchExtensions = {},
      .standalone = true};
  std::vector<std::string> files;
  const auto add = [](std::vector<std::string>& to,
                      std::span<const std::string> more) {
    to.insert(to.end(), more.begin(), more.end());
  };
  for (const SourceSet* set : library.sets) {
    add(files, set->files);
    add(how.includePaths, set->incdir);
    add(how.undefines, set->undefines);
    add(how.searchDirectories, set->searchdir);
    add(how.searchExtensions, set->searchext);
    // A directory given to a unit alone is not looked at until an include is
    // searched for, so one that does not exist would otherwise go unreported.
    for (const auto& dir : set->incdir) {
      std::error_code ec;
      if (!std::filesystem::is_directory(dir, ec)) {
        return diag::Fail(
            diag::DiagCode::kHostInvalidManifest,
            std::format(
                "{}: include directory '{}': not a directory",
                manifest.path.string(), dir));
      }
    }
  }
  for (const SourceSet* set : library.sets | std::views::reverse) {
    add(how.defines, set->defines);
  }
  add(how.includePaths, manifest.library.export_incdir);

  const std::span<const std::string> all_files = files;
  const std::size_t files_per_unit = library.single_unit ? files.size() : 1;
  for (std::size_t at = 0; at < files.size(); at += files_per_unit) {
    driver.sourceLoader.addSeparateUnit(
        all_files.subspan(at, files_per_unit), how);
  }
  return AddIncludeDirectories(
      manifest, manifest.library.export_incdir, driver);
}

}  // namespace

auto ApplyDeclarations(
    const Declarations& declared, slang::driver::Driver& driver)
    -> diag::Result<void> {
  const Manifest& root = declared.root;
  InvocationMaterial invocation{
      .defines = driver.options.defines,
      .undefines = driver.options.undefines,
      .incdir = {}};
  for (const auto& dir : driver.sourceManager.getUserDirectories()) {
    invocation.incdir.push_back(dir.string());
  }

  // Every cell compiled here belongs to the declared library: it is this
  // build's default library (LRM 33.3.1), under its own name.
  if (!driver.options.defaultLibName) {
    driver.options.defaultLibName = root.library.name;
  }
  driver.options.libraryLiblists[*driver.options.defaultLibName] =
      LibraryListOf(root, *driver.options.defaultLibName);
  if (auto ok = AddLibrary(
          {.declaration = &root,
           .sets = {&root.library.sources, &root.design.sources},
           .front_end_library = "",
           .single_unit = driver.options.singleUnit.value_or(
               root.single_unit.value_or(false))},
          invocation, driver);
      !ok) {
    return std::unexpected(std::move(ok.error()));
  }
  for (const Manifest& reached : declared.dependencies) {
    driver.options.libraryLiblists[reached.library.name] =
        LibraryListOf(reached, reached.library.name);
    if (auto ok = AddLibrary(
            {.declaration = &reached,
             .sets = {&reached.library.sources},
             .front_end_library = reached.library.name,
             .single_unit = reached.single_unit.value_or(false)},
            invocation, driver);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
  }

  // The front end keeps the first override it is given of a parameter, so the
  // command line's go ahead of the declared ones.
  driver.options.paramOverrides.insert(
      driver.options.paramOverrides.end(), root.design.params.begin(),
      root.design.params.end());
  if (driver.options.topModules.empty()) {
    driver.options.topModules = root.design.top;
  }
  if (!driver.options.languageVersion) {
    driver.options.languageVersion = root.language_version;
  }
  if (!driver.options.timeScale) {
    driver.options.timeScale = root.timescale;
  }
  return {};
}

auto ResolveCliOptions(
    const CliOptions& opts, const Declarations* declared, CommandKind cmd,
    std::span<const std::string> simulation_args)
    -> std::expected<ParsedArgs, std::string> {
  ParsedArgs out;
  out.simulation_args.assign(simulation_args.begin(), simulation_args.end());
  out.formatting = opts.format.value_or(false) ? driver::SourceFormatting::kOn
                                               : driver::SourceFormatting::kOff;
  out.optimization = opts.release.value_or(false)
                         ? driver::Optimization::kRelease
                         : driver::Optimization::kIterate;
  out.pch = opts.no_pch.value_or(false) ? driver::pch::Policy::kSkip
                                        : driver::pch::Policy::kAttempt;
  out.cxx = opts.cxx;
  auto width = ResolveCompileWidth(opts.jobs);
  if (!width) {
    return std::unexpected(std::move(width.error()));
  }
  out.compile_width = *width;
  if (opts.out && !opts.out->empty()) {
    out.out = std::filesystem::path(*opts.out);
  }
  out.store = driver::LocateStore(
      opts.cache_dir && !opts.cache_dir->empty()
          ? std::optional<std::filesystem::path>{*opts.cache_dir}
          : std::nullopt);
  out.rebuild = opts.rebuild.value_or(false);

  // The declared foreign sources are the base, a library's before those of what
  // depends on it; the command line's are extras this invocation adds, so they
  // follow.
  if (declared != nullptr) {
    const Manifest& root = declared->root;
    out.library_name = root.library.name;
    const auto link = [&](const SourceSet& sources) {
      out.dpi_link_sources.insert(
          out.dpi_link_sources.end(), sources.dpi.begin(), sources.dpi.end());
    };
    for (const Manifest& reached : declared->dependencies) {
      link(reached.library.sources);
    }
    link(root.library.sources);
    link(root.design.sources);
    // Whether an assertion is checked changes nothing the design computes, so
    // it is the build's to say and the root's answer covers every library.
    if (root.assertions) {
      out.assertions = *root.assertions;
    }
  }
  out.dpi_link_sources.insert(
      out.dpi_link_sources.end(), opts.dpi_link.begin(), opts.dpi_link.end());

  if (opts.assertions) {
    auto policy = ParseAssertionPolicy(*opts.assertions);
    if (!policy) {
      return std::unexpected(
          std::format(
              "--assertions: '{}' is not one of check, skip",
              *opts.assertions));
    }
    out.assertions = *policy;
  }

  if (opts.backend) {
    auto backend = ParseBackend(*opts.backend);
    if (!backend) {
      return std::unexpected(
          std::format(
              "--backend: '{}' is not one of cpp, llvm", *opts.backend));
    }
    out.backend = *backend;
  }

  if (RequiresOut(cmd) && !out.out) {
    return std::unexpected(
        std::format("{} requires --out\n{}", CommandSpelling(cmd), Usage()));
  }
  return out;
}

}  // namespace lyra::cli
