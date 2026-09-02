#include "lyra/cli/command_line.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdlib>
#include <expected>
#include <filesystem>
#include <format>
#include <iterator>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <unistd.h>
#include <utility>
#include <vector>

#include <fmt/core.h>
#include <slang/driver/Driver.h>
#include <slang/util/CommandLine.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/cli/design_manifest.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace lyra::cli {

namespace {

// Every command in one place: how it is spelled, and the one fact a command
// decides rather than inherits. A command is named by a verb and an object,
// with an empty object for a verb that stands alone. The usage text, the parse,
// and the output-directory check all read this table, so adding a command is
// one row rather than four lists that drift apart.
struct CommandSpec {
  std::string_view verb;
  std::string_view object;
  CommandKind kind;
  bool requires_out_dir;
};

// Entries sharing a verb stay adjacent: the usage line groups them by that
// adjacency into `dump hir|mir|lir|llvm`.
constexpr auto kCommands = std::to_array<CommandSpec>(
    {{.verb = "check",
      .object = "",
      .kind = CommandKind::kCheck,
      .requires_out_dir = false},
     {.verb = "dump",
      .object = "ast",
      .kind = CommandKind::kDumpAst,
      .requires_out_dir = false},
     {.verb = "dump",
      .object = "hir",
      .kind = CommandKind::kDumpHir,
      .requires_out_dir = false},
     {.verb = "dump",
      .object = "mir",
      .kind = CommandKind::kDumpMir,
      .requires_out_dir = false},
     {.verb = "dump",
      .object = "lir",
      .kind = CommandKind::kDumpLir,
      .requires_out_dir = false},
     {.verb = "dump",
      .object = "llvm",
      .kind = CommandKind::kDumpLlvm,
      .requires_out_dir = false},
     {.verb = "emit",
      .object = "cpp",
      .kind = CommandKind::kEmitCpp,
      .requires_out_dir = true},
     {.verb = "compile",
      .object = "",
      .kind = CommandKind::kCompile,
      .requires_out_dir = true},
     {.verb = "run",
      .object = "",
      .kind = CommandKind::kRun,
      .requires_out_dir = false},
     {.verb = "cache",
      .object = "clear",
      .kind = CommandKind::kCacheClear,
      .requires_out_dir = false}});

// The spelling of each execution backend on the command line. Returns nullopt
// for a name outside the table, which is a user typing a value the command
// line cannot restrict rather than a drift between two internal lists.
auto ParseBackend(std::string_view name) -> std::optional<Backend> {
  static constexpr std::array<std::pair<std::string_view, Backend>, 4> kNames =
      {{{"cpp", Backend::kCpp},
        {"jit", Backend::kJit},
        {"aot", Backend::kAot},
        {"lli", Backend::kLli}}};
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
auto RequiresOutDir(CommandKind cmd) -> bool {
  return FindCommand(cmd).requires_out_dir;
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
      "disable the precompiled-header cache for this invocation");
  cmd.add(
      "--pch-cache-dir", opts.pch_cache_dir, "override the PCH cache directory",
      "<dir>", slang::CommandLineFlags::FilePath);
  cmd.add(
      "--cxx", opts.cxx,
      "host C++ compiler for the C++ backend: a path, or a name found on PATH",
      "<program>");
  cmd.add(
      "-o,--out-dir", opts.out_dir, "write output to this directory", "<dir>",
      slang::CommandLineFlags::FilePath);
  cmd.add(
      "--backend", opts.backend, "how `run` executes the design",
      "cpp|jit|aot|lli");
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

auto MakePchOptions(const CliOptions& cli) -> driver::pch::Options {
  driver::pch::Options opts;
  opts.disabled = cli.no_pch.value_or(false);
  if (const char* v = std::getenv("LYRA_NO_PCH");
      v != nullptr && *v != '\0' && std::string_view(v) != "0") {
    opts.disabled = true;
  }
  if (cli.pch_cache_dir && !cli.pch_cache_dir->empty()) {
    opts.cache_dir_override = std::filesystem::path(*cli.pch_cache_dir);
  }
  return opts;
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
    std::vector<std::string> child_args)
    -> std::expected<ParsedArgs, std::string> {
  ParsedArgs out;
  out.cmd = cmd;
  out.child_args = std::move(child_args);
  out.format = opts.format.value_or(false);
  out.optimization = opts.release.value_or(false)
                         ? driver::Optimization::kRelease
                         : driver::Optimization::kIterate;
  out.pch = MakePchOptions(opts);
  out.cxx = opts.cxx.value_or("clang++");
  out.out_dir = opts.out_dir.value_or("");

  // The design's own foreign sources are the base; the command line's are
  // extras this invocation adds, so they follow.
  if (manifest != nullptr) {
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
              "--backend: '{}' is not one of cpp, jit, aot, lli",
              *opts.backend));
    }
    out.backend = *backend;
  }

  if (RequiresOutDir(cmd) && out.out_dir.empty()) {
    return std::unexpected(
        std::format(
            "{} requires --out-dir\n{}", CommandSpelling(cmd), Usage()));
  }
  return out;
}

}  // namespace lyra::cli
