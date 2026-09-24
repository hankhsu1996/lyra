#include "lyra/cli/command_line.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
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
#include "lyra/cli/design_manifest.hpp"
#include "lyra/driver/artifact_store.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
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

// What every command reading a design acts on: how its report is coloured, and
// where the design's declaration is.
constexpr OptionSet kReadsADesign = {
    LyraOption::kColor, LyraOption::kNoColor, LyraOption::kConfig};
// What every command lowering the design acts on besides: the policy lowering
// follows.
constexpr OptionSet kLowersADesign =
    kReadsADesign | OptionSet{LyraOption::kAssertions};
// What building the design's program acts on besides.
constexpr OptionSet kBuildsAProgram =
    kLowersADesign | OptionSet{LyraOption::kRelease,  LyraOption::kNoPch,
                               LyraOption::kCacheDir, LyraOption::kRebuild,
                               LyraOption::kCxx,      LyraOption::kJobs,
                               LyraOption::kBackend,  LyraOption::kDpiLink};

// Every command in one place: how it is spelled, and the facts a command
// decides rather than inherits -- whether it needs somewhere to write, and
// which options it acts on. A command is named by a verb and an object, with an
// empty object for a verb that stands alone. The usage text, the parse, the
// output check, and the check that refuses an option a command does not act on
// all read this table, so adding a command is one row that has to say all of
// it, rather than several lists that drift apart.
struct CommandSpec {
  std::string_view verb;
  std::string_view object;
  CommandKind kind;
  bool requires_out;
  OptionSet takes;
};

// Entries sharing a verb stay adjacent: the usage line groups them by that
// adjacency into `dump hir|mir|lir|llvm`.
constexpr auto kCommands = std::to_array<CommandSpec>(
    {{.verb = "check",
      .object = "",
      .kind = CommandKind::kCheck,
      .requires_out = false,
      .takes = kReadsADesign},
     {.verb = "dump",
      .object = "ast",
      .kind = CommandKind::kDumpAst,
      .requires_out = false,
      .takes = kReadsADesign},
     {.verb = "dump",
      .object = "hir",
      .kind = CommandKind::kDumpHir,
      .requires_out = false,
      .takes = kLowersADesign},
     {.verb = "dump",
      .object = "mir",
      .kind = CommandKind::kDumpMir,
      .requires_out = false,
      .takes = kLowersADesign},
     {.verb = "dump",
      .object = "lir",
      .kind = CommandKind::kDumpLir,
      .requires_out = false,
      .takes = kLowersADesign},
     {.verb = "dump",
      .object = "llvm",
      .kind = CommandKind::kDumpLlvm,
      .requires_out = false,
      .takes = kLowersADesign},
     // The project's recipe is written with the compiler and the level it
     // compiles at baked in, and takes its own width and precompiled header
     // when it is run.
     {.verb = "emit",
      .object = "cpp",
      .kind = CommandKind::kEmitCpp,
      .requires_out = true,
      .takes = kLowersADesign |
               OptionSet{
                   LyraOption::kFormat, LyraOption::kRelease, LyraOption::kCxx,
                   LyraOption::kOut, LyraOption::kDpiLink}},
     {.verb = "build",
      .object = "",
      .kind = CommandKind::kBuild,
      .requires_out = false,
      .takes = kBuildsAProgram | OptionSet{LyraOption::kOut}},
     {.verb = "run",
      .object = "",
      .kind = CommandKind::kRun,
      .requires_out = false,
      .takes = kBuildsAProgram | OptionSet{LyraOption::kSimulationArgs}},
     {.verb = "cache",
      .object = "clear",
      .kind = CommandKind::kCacheClear,
      .requires_out = false,
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
      "--config", opts.config,
      "read this design declaration instead of searching for one", "<file>",
      slang::CommandLineFlags::FilePath);
  cmd.add(
      "--release", opts.release,
      "optimize the simulation rather than the time to build it");
  cmd.add(
      "--no-pch", opts.no_pch,
      "compile the C++ backend's output without a precompiled header");
  cmd.add(
      "--cache-dir", opts.cache_dir,
      "where built programs and prepared headers are kept for reuse", "<dir>",
      slang::CommandLineFlags::FilePath);
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
      "how many of the design's translation units to compile at once; "
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
    auto loaded = LoadDesignManifest(path);
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
  auto search = FindDesignManifest(here);
  if (const auto* absent = std::get_if<ManifestAbsent>(&search)) {
    return DesignDeclaration{*absent};
  }
  return load(std::get<ManifestFound>(search).path);
}

auto ApplyDesignManifest(
    const DesignManifest& manifest, slang::driver::Driver& driver)
    -> diag::Result<void> {
  for (const auto& file : manifest.files) {
    driver.sourceLoader.addFiles(file);
  }
  // Added after the command line's, which is what keeps a search path given
  // there ahead of the design's own: the first directory holding a file wins.
  for (const auto& dir : manifest.incdir) {
    if (const std::error_code ec =
            driver.sourceManager.addUserDirectories(dir)) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidManifest,
          std::format(
              "{}: include directory '{}': {}", manifest.path.string(), dir,
              ec.message()));
    }
  }
  for (const auto& dir : manifest.libdir) {
    driver.sourceLoader.addSearchDirectories(dir);
  }
  for (const auto& extension : manifest.libext) {
    driver.sourceLoader.addSearchExtension(extension);
  }
  // Inserted ahead of the command line's, for the same reason read from the
  // other end: the last definition of a macro, or of a parameter, stands.
  driver.options.defines.insert(
      driver.options.defines.begin(), manifest.defines.begin(),
      manifest.defines.end());
  driver.options.undefines.insert(
      driver.options.undefines.begin(), manifest.undefines.begin(),
      manifest.undefines.end());
  driver.options.paramOverrides.insert(
      driver.options.paramOverrides.begin(), manifest.params.begin(),
      manifest.params.end());
  if (driver.options.topModules.empty()) {
    driver.options.topModules = manifest.top;
  }
  if (!driver.options.languageVersion) {
    driver.options.languageVersion = manifest.language_version;
  }
  if (!driver.options.timeScale) {
    driver.options.timeScale = manifest.timescale;
  }
  if (!driver.options.singleUnit) {
    driver.options.singleUnit = manifest.single_unit;
  }
  return {};
}

auto ResolveCliOptions(
    const CliOptions& opts, const DesignManifest* manifest, CommandKind cmd,
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

  // The design's own foreign sources are the base; the command line's are
  // extras this invocation adds, so they follow.
  if (manifest != nullptr) {
    out.design_name = manifest->name;
    out.dpi_link_sources = manifest->dpi_sources;
    if (manifest->assertions) {
      out.assertions = *manifest->assertions;
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
