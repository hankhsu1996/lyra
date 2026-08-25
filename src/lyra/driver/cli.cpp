#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <exception>
#include <expected>
#include <filesystem>
#include <format>
#include <iostream>
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
#include <slang/ast/ASTSerializer.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/driver/Driver.h>
#include <slang/text/Json.h>
#include <slang/util/CommandLine.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/render.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/driver/cpp_build.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/jit/executor.hpp"
#include "lyra/lir/dump.hpp"
#include "lyra/mir/dump.hpp"
#include "lyra/support/subprocess.hpp"

namespace {

enum class CommandKind : std::uint8_t {
  kCheck,
  kDumpAst,
  kDumpHir,
  kDumpMir,
  kDumpLir,
  kDumpLlvm,
  kEmitCpp,
  kCompile,
  kRun,
  kCacheClear,
};

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

// How `run` executes the design. The C++ backend emits a C++ project and builds
// it; the LLVM backends share one emitted module and differ only in how they
// run it (in-process ORC JIT, ahead-of-time native compile, or the `lli` tool).
enum class Backend : std::uint8_t { kCpp, kJit, kAot, kLli };

// Whether diagnostics carry ANSI colour. `kAuto` asks the terminal; the other
// two are the user overriding that answer in either direction.
enum class ColorPreference : std::uint8_t { kAuto, kAlways, kNever };

struct ParsedArgs {
  CommandKind cmd = CommandKind::kEmitCpp;
  ColorPreference color = ColorPreference::kAuto;
  Backend backend = Backend::kCpp;
  lyra::compiler::LoweringPolicy lowering;
  bool no_project = false;
  bool format = false;
  bool no_pch = false;
  std::string pch_cache_dir;
  // The host C++ compiler the C++ backend builds emitted code with: a program
  // name or path, never flags.
  std::string cxx;
  std::string out_dir;
  // Forwarded verbatim as the built program's argv, which is where LRM 21.6
  // plusargs land. Everything after a standalone `--`, so a simulation
  // argument never has to be told apart from a compiler one by its spelling.
  std::vector<std::string> child_args;
  // LRM 35 DPI-C link inputs: native source files (`.c` / `.cpp`) providing the
  // foreign symbols an `import "DPI-C"` calls. Compiled and linked into the
  // built program alongside the emitted C++.
  std::vector<std::string> dpi_link_sources;
};

// What Lyra adds to the front end's command line. Every field is optional
// because that is how the parser says "not given", which is a different fact
// from the default Lyra then chooses.
struct CliOptions {
  std::optional<bool> no_project;
  std::optional<bool> color;
  std::optional<bool> no_color;
  std::optional<bool> format;
  std::optional<bool> disable_assertions;
  std::optional<bool> no_pch;
  std::optional<std::string> pch_cache_dir;
  std::optional<std::string> cxx;
  std::optional<std::string> out_dir;
  std::optional<std::string> backend;
  std::vector<std::string> dpi_link;
};

// Registers Lyra's own options beside the front end's on one command line, so
// a build that already knows how to describe a design to slang describes it to
// Lyra the same way, and one help text covers both.
void RegisterCliOptions(slang::CommandLine& cmd, CliOptions& opts) {
  cmd.add(
      "--no-project", opts.no_project,
      "operate in direct file mode (no lyra.toml lookup)");
  cmd.add(
      "--color", opts.color,
      "force ANSI color in diagnostics, overriding TTY detection");
  cmd.add("--no-color", opts.no_color, "disable ANSI color in diagnostics");
  cmd.add(
      "--format", opts.format,
      "reformat the emitted C++ with clang-format (skipped if absent)");
  cmd.add(
      "--disable-assertions", opts.disable_assertions,
      "skip assertion constructs during lowering instead of rejecting them");
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

auto FindCommand(CommandKind cmd) -> const CommandSpec& {
  const auto* const it = std::ranges::find(kCommands, cmd, &CommandSpec::kind);
  if (it == kCommands.end()) {
    throw lyra::InternalError("command kind is absent from the command table");
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

// Turns what the parser recorded into the choices the rest of the run needs,
// applying Lyra's defaults and rejecting a value no command can act on.
auto ResolveCliOptions(
    const CliOptions& opts, CommandKind cmd,
    std::vector<std::string> child_args)
    -> std::expected<ParsedArgs, std::string> {
  ParsedArgs out;
  out.cmd = cmd;
  out.child_args = std::move(child_args);
  out.no_project = opts.no_project.value_or(false);
  out.format = opts.format.value_or(false);
  out.no_pch = opts.no_pch.value_or(false);
  out.lowering.disable_assertions = opts.disable_assertions.value_or(false);
  out.pch_cache_dir = opts.pch_cache_dir.value_or("");
  out.cxx = opts.cxx.value_or("clang++");
  out.out_dir = opts.out_dir.value_or("");
  out.dpi_link_sources = opts.dpi_link;

  if (opts.no_color.value_or(false)) {
    out.color = ColorPreference::kNever;
  } else if (opts.color.value_or(false)) {
    out.color = ColorPreference::kAlways;
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

auto UseColor(ColorPreference pref) -> bool {
  switch (pref) {
    case ColorPreference::kNever:
      return false;
    case ColorPreference::kAlways:
      return true;
    case ColorPreference::kAuto:
      return ::isatty(STDERR_FILENO) != 0;
  }
  return false;
}

// Turns a diagnostic into terminal output. Constructed once, after the
// terminal has been inspected, and handed to every command so none of them
// re-decides how rendering works.
class Reporter {
 public:
  explicit Reporter(lyra::diag::RenderOptions opts) : opts_(opts) {
  }

  void operator()(
      lyra::diag::Diagnostic diag,
      const lyra::diag::SourceManager* mgr = nullptr) const {
    fmt::print(stderr, "{}", lyra::diag::RenderDiagnostic(diag, mgr, opts_));
  }

  void operator()(
      const lyra::diag::DiagnosticSink& sink,
      const lyra::diag::SourceManager* mgr) const {
    fmt::print(stderr, "{}", lyra::diag::RenderDiagnostics(sink, mgr, opts_));
  }

 private:
  lyra::diag::RenderOptions opts_;
};

// PCH policy condensed into one explicit value. The `--no-pch` flag is
// authoritative; the `LYRA_NO_PCH` environment hint is honored at this
// boundary only and disappears from every layer below.
auto MakePchOptions(const ParsedArgs& args) -> lyra::driver::pch::Options {
  lyra::driver::pch::Options opts;
  opts.disabled = args.no_pch;
  if (const char* v = std::getenv("LYRA_NO_PCH");
      v != nullptr && *v != '\0' && std::string_view(v) != "0") {
    opts.disabled = true;
  }
  if (!args.pch_cache_dir.empty()) {
    opts.cache_dir_override = std::filesystem::path(args.pch_cache_dir);
  }
  return opts;
}

// What a command receives: the request, what the compiler produced from it,
// and the channel for anything that goes wrong. A command reads this and
// returns the process exit code; nothing else about the invocation is visible
// to it.
// Members are non-owning pointers rather than references: this outlives
// nothing, and a reference member would make the type unassignable for no gain.
struct CommandContext {
  const ParsedArgs* args;
  const lyra::compiler::CompileArtifacts* artifacts;
  const lyra::diag::SourceManager* mgr;
  std::span<const lyra::driver::DpiLinkInput> dpi_inputs;
  lyra::driver::SourceFormatting formatting;
  const Reporter* report;
  std::string_view program_path;
};

// Where the bundled runtime headers and archive live, relative to this
// executable. Reports and returns nullopt when they cannot be found, so a
// caller only has to leave.
auto ResolveRuntime(const CommandContext& ctx)
    -> std::optional<lyra::driver::RuntimeLocation> {
  auto loc_or =
      lyra::driver::ResolveRuntimeLocation(std::string(ctx.program_path));
  if (!loc_or) {
    (*ctx.report)(lyra::diag::Make(
        lyra::diag::DiagCode::kHostIoError, std::move(loc_or.error())));
    return std::nullopt;
  }
  return *std::move(loc_or);
}

// Resolved here rather than up front because `dump` must keep working on a
// machine with no C++ compiler installed: a missing compiler is fatal only to
// the commands that would invoke one.
auto ResolveHostBuild(const CommandContext& ctx)
    -> std::optional<lyra::driver::HostBuild> {
  auto cxx_or = lyra::support::FindOnPath(ctx.args->cxx);
  if (!cxx_or) {
    (*ctx.report)(lyra::diag::Make(
        lyra::diag::DiagCode::kHostIoError, std::move(cxx_or.error())));
    return std::nullopt;
  }
  return lyra::driver::HostBuild{
      .cxx = *std::move(cxx_or), .pch = MakePchOptions(*ctx.args)};
}

// How far the compiler has to lower for a command to have what it reads.
// Exhaustive on purpose: a new command must state its own depth rather than
// inherit one silently.
auto LoweringDepth(const ParsedArgs& args) -> lyra::compiler::StopAfter {
  switch (args.cmd) {
    case CommandKind::kCheck:
    case CommandKind::kDumpAst:
      return lyra::compiler::StopAfter::kParse;
    case CommandKind::kDumpHir:
      return lyra::compiler::StopAfter::kHir;
    case CommandKind::kDumpLir:
    case CommandKind::kDumpLlvm:
      return lyra::compiler::StopAfter::kLir;
    case CommandKind::kRun:
      return args.backend == Backend::kCpp ? lyra::compiler::StopAfter::kMir
                                           : lyra::compiler::StopAfter::kLir;
    case CommandKind::kDumpMir:
    case CommandKind::kEmitCpp:
    case CommandKind::kCompile:
    case CommandKind::kCacheClear:
      return lyra::compiler::StopAfter::kMir;
  }
  return lyra::compiler::StopAfter::kMir;
}

// The front end's own account of the design, upstream of every form Lyra
// derives from it.
//
// One writer lives across the whole run, because state it carries between
// values -- which enum types have already been printed -- has to stay
// consistent. Flushing after each complete top-level value then keeps peak
// memory proportional to the largest single object rather than to the design,
// which is what makes a design of any size dumpable at all.
auto RunDumpAst(const CommandContext& ctx) -> int {
  slang::JsonWriter writer;
  writer.setPrettyPrint(true);

  slang::ast::Compilation& compilation = ctx.artifacts->Elaboration();
  slang::ast::ASTSerializer serializer(compilation, writer);
  serializer.setTryConstantFold(false);

  serializer.startObject();
  serializer.writeProperty("design");
  serializer.serialize(compilation.getRoot());
  writer.flushTo(std::cout);

  serializer.writeProperty("definitions");
  serializer.startArray();
  for (const auto* definition : compilation.getDefinitions()) {
    serializer.serialize(*definition);
    writer.flushTo(std::cout);
  }
  serializer.endArray();
  serializer.endObject();

  writer.writeNewLine();
  writer.flushTo(std::cout);
  return 0;
}

auto RunDumpHir(const CommandContext& ctx) -> int {
  fmt::print("{}", lyra::hir::DumpHir(ctx.artifacts->HirUnits()));
  return 0;
}

auto RunDumpMir(const CommandContext& ctx) -> int {
  for (const auto& unit : ctx.artifacts->MirUnits()) {
    fmt::print("{}", lyra::mir::DumpMir(unit));
  }
  fmt::print("{}", lyra::mir::DumpMir(ctx.artifacts->RootUnit()));
  return 0;
}

auto RunDumpLir(const CommandContext& ctx) -> int {
  for (const auto& unit : ctx.artifacts->LirUnits()) {
    fmt::print("{}", lyra::lir::DumpLir(unit));
  }
  fmt::print("{}", lyra::lir::DumpLir(ctx.artifacts->RootLirUnit()));
  return 0;
}

auto RunDumpLlvm(const CommandContext& ctx) -> int {
  for (const auto& unit : ctx.artifacts->LirUnits()) {
    fmt::print("{}", lyra::backend::llvm_backend::EmitModule(unit).Print());
  }
  fmt::print(
      "{}",
      lyra::backend::llvm_backend::EmitModule(ctx.artifacts->RootLirUnit())
          .Print());
  return 0;
}

// Writes the portable project `emit cpp` produces and `compile` then builds.
// Both need the same runtime and host build, so both get them from here and
// neither restates the assembly.
auto AssemblePortableProject(const CommandContext& ctx)
    -> std::optional<lyra::driver::HostBuild> {
  auto runtime = ResolveRuntime(ctx);
  if (!runtime) {
    return std::nullopt;
  }
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return std::nullopt;
  }
  auto assembled = lyra::driver::AssembleProject(
      *runtime, ctx.artifacts->MirUnits(), ctx.artifacts->RootUnit(),
      ctx.args->out_dir, ctx.formatting, *host, ctx.dpi_inputs);
  if (!assembled) {
    (*ctx.report)(std::move(assembled.error()), ctx.mgr);
    return std::nullopt;
  }
  return host;
}

auto RunEmitCpp(const CommandContext& ctx) -> int {
  if (!AssemblePortableProject(ctx)) {
    return 1;
  }
  fmt::print("emitted: {}\n", ctx.args->out_dir);
  return 0;
}

auto RunCompile(const CommandContext& ctx) -> int {
  auto host = AssemblePortableProject(ctx);
  if (!host) {
    return 1;
  }
  auto built =
      lyra::driver::BuildProject(ctx.args->out_dir, *host, ctx.dpi_inputs);
  if (!built) {
    (*ctx.report)(std::move(built.error()), ctx.mgr);
    return 1;
  }
  fmt::print("compiled: {}\n", built->string());
  return 0;
}

auto RunCppBackend(const CommandContext& ctx) -> int {
  auto runtime = ResolveRuntime(ctx);
  if (!runtime) {
    return 1;
  }
  auto work_dir = lyra::support::MakeTempDir();
  if (!work_dir) {
    (*ctx.report)(lyra::diag::Make(
        lyra::diag::DiagCode::kHostIoError, std::move(work_dir.error())));
    return 1;
  }
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return 1;
  }
  auto exit_code = lyra::driver::RunInPlace(
      *runtime, ctx.artifacts->MirUnits(), ctx.artifacts->RootUnit(), *work_dir,
      ctx.formatting, *host, ctx.args->child_args, ctx.dpi_inputs);
  if (!exit_code) {
    (*ctx.report)(std::move(exit_code.error()), ctx.mgr);
    return 1;
  }
  return *exit_code;
}

// A JIT image has no link step, so the design's DPI-C sources are compiled
// into a library the execution session resolves the imports' foreign symbols
// from. The temp directory holds that library and the ABI header the sources
// compile against. A design with no foreign sources needs neither.
auto BuildJitDpiLibrary(const CommandContext& ctx)
    -> std::optional<std::optional<std::filesystem::path>> {
  if (ctx.dpi_inputs.empty()) {
    return std::optional<std::filesystem::path>{};
  }
  auto runtime = ResolveRuntime(ctx);
  if (!runtime) {
    return std::nullopt;
  }
  auto dir = lyra::support::MakeTempDir();
  if (!dir) {
    (*ctx.report)(lyra::diag::Make(
        lyra::diag::DiagCode::kHostIoError, std::move(dir.error())));
    return std::nullopt;
  }
  if (auto surface = lyra::driver::WriteDpiSurface(
          *runtime, ctx.artifacts->MirUnits(), ctx.artifacts->RootUnit(), *dir);
      !surface) {
    (*ctx.report)(std::move(surface.error()), ctx.mgr);
    return std::nullopt;
  }
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return std::nullopt;
  }
  auto built = lyra::driver::BuildDpiSharedLibrary(
      ctx.dpi_inputs, host->cxx, *dir, *dir);
  if (!built) {
    (*ctx.report)(std::move(built.error()), ctx.mgr);
    return std::nullopt;
  }
  return std::optional<std::filesystem::path>{*std::move(built)};
}

auto RunJitBackend(const CommandContext& ctx) -> int {
  auto dpi_library = BuildJitDpiLibrary(ctx);
  if (!dpi_library) {
    return 1;
  }
  // The design-root unit's construct elaborates the whole design, building the
  // top-level units as its owned children, so the JIT runs the design once from
  // that one entry rather than per top.
  return lyra::jit::Execute(
      ctx.artifacts->LirUnits(), ctx.artifacts->UnitMetadata(),
      ctx.artifacts->RootLirUnit(), ctx.artifacts->RootMetadata(),
      *dpi_library);
}

auto RunBackend(const CommandContext& ctx) -> int {
  switch (ctx.args->backend) {
    case Backend::kCpp:
      return RunCppBackend(ctx);
    case Backend::kJit:
      return RunJitBackend(ctx);
    case Backend::kAot:
    case Backend::kLli:
      (*ctx.report)(lyra::diag::Make(
          lyra::diag::DiagCode::kHostBackendUnimplemented,
          "this execution backend is not yet implemented"));
      return 1;
  }
  return 1;
}

auto RunCommand(const CommandContext& ctx) -> int {
  switch (ctx.args->cmd) {
    case CommandKind::kCheck:
      // The front end has already run and everything it had to say has already
      // been reported, so arriving here is the whole answer `check` gives.
      return 0;
    case CommandKind::kDumpAst:
      return RunDumpAst(ctx);
    case CommandKind::kDumpHir:
      return RunDumpHir(ctx);
    case CommandKind::kDumpMir:
      return RunDumpMir(ctx);
    case CommandKind::kDumpLir:
      return RunDumpLir(ctx);
    case CommandKind::kDumpLlvm:
      return RunDumpLlvm(ctx);
    case CommandKind::kEmitCpp:
      return RunEmitCpp(ctx);
    case CommandKind::kCompile:
      return RunCompile(ctx);
    case CommandKind::kRun:
      return RunBackend(ctx);
    case CommandKind::kCacheClear:
      break;
  }
  throw lyra::InternalError("cache clear reached the compiling dispatch");
}

// What Lyra parses and what it hands the simulation, told apart by a standalone
// `--`. Splitting there keeps the front end from ever having to decide whether
// a `+`-prefixed word is one of its options or one of the design's plusargs.
struct SplitArgv {
  std::vector<char*> lyra;
  std::vector<std::string> child;
};

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

// The one merged option list: Lyra's own options and every front-end option
// inherited alongside them. Asking the parser to render it is what keeps the
// help honest about which options a build may actually pass.
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

// One pass over Lyra's side of the command line: the command words first --
// they are positional -- then the options, through the parser both halves
// share. `words` is consumed: the command words are removed from it.
auto ParseCommandLine(
    slang::driver::Driver& driver, const CliOptions& cli_options,
    std::vector<char*>& words, std::vector<std::string> child_args)
    -> std::expected<ParsedArgs, std::string> {
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
  return ResolveCliOptions(cli_options, command->first, std::move(child_args));
}

}  // namespace

auto main(int argc, char** argv) -> int {
  try {
    const std::span<char* const> raw_args(argv, static_cast<std::size_t>(argc));
    const std::string program_path =
        raw_args.empty() ? std::string{} : std::string(raw_args.front());
    auto argv_split = SplitAtSeparator(raw_args);

    slang::driver::Driver driver;
    driver.addStandardArgs();
    CliOptions cli_options;
    RegisterCliOptions(driver.cmdLine, cli_options);

    if (WantsHelp(argv_split.lyra)) {
      PrintHelp(driver);
      return 0;
    }

    auto parsed = ParseCommandLine(
        driver, cli_options, argv_split.lyra, std::move(argv_split.child));

    const bool use_color =
        UseColor(parsed.has_value() ? parsed->color : ColorPreference::kAuto);
    const Reporter report{lyra::diag::RenderOptions{
        .use_color = use_color, .show_source_snippet = true}};

    if (!parsed) {
      // An empty message means the parser already printed its own account of
      // what was wrong with the command line.
      if (!parsed.error().empty()) {
        report(
            lyra::diag::Make(
                lyra::diag::DiagCode::kHostInvalidCliArgs, parsed.error()));
      }
      return 1;
    }
    const auto& args = *parsed;
    driver.setTerminalColorsEnabled(use_color);

    // `cache clear` consults no project, takes no input files, and never
    // reaches the compiler. Dispatch it before the input checks below so it
    // works whether or not a project is configured.
    if (args.cmd == CommandKind::kCacheClear) {
      auto cleared_or = lyra::driver::pch::Clear(MakePchOptions(args));
      if (!cleared_or) {
        report(std::move(cleared_or.error()));
        return 1;
      }
      fmt::print(
          "cleared {} precompiled-header file{}\n", *cleared_or,
          *cleared_or == 1 ? "" : "s");
      return 0;
    }

    if (!args.no_project) {
      report(
          lyra::diag::Make(
              lyra::diag::DiagCode::kHostProjectModeUnimplemented,
              "project mode is not implemented yet; pass --no-project to "
              "run in direct file mode"));
      return 1;
    }
    if (!driver.sourceLoader.hasFiles()) {
      report(
          lyra::diag::Make(
              lyra::diag::DiagCode::kHostNoInputFiles, "no input files"));
      return 1;
    }

    // Classified before compiling anything, so a mistyped path is reported
    // against the command line rather than after a full frontend and lowering
    // pass.
    auto dpi_inputs =
        lyra::driver::ValidateDpiLinkInputs(args.dpi_link_sources);
    if (!dpi_inputs) {
      report(std::move(dpi_inputs.error()));
      return 1;
    }

    lyra::diag::DiagnosticSink sink;
    auto result = lyra::compiler::Compile(
        driver, args.lowering, sink, LoweringDepth(args));

    // `run` executes the simulation; its stdout/stderr are the simulation's
    // own, so compile-phase warnings must not bleed into them. Surface slang
    // diagnostics for run only when they carry errors (which abort below);
    // other commands always show them. Use `compile`/`dump` to see warnings.
    const bool suppress_compile_warnings =
        args.cmd == CommandKind::kRun && result.slang_ok && !sink.HasErrors();
    if (!suppress_compile_warnings && !result.slang_diagnostics.empty()) {
      fmt::print(stderr, "{}", result.slang_diagnostics);
    }

    const lyra::diag::SourceManager* mgr =
        result.artifacts.parse ? &result.artifacts.parse->diag_sources
                               : nullptr;
    if (sink.HasErrors()) {
      report(sink, mgr);
      return 1;
    }
    if (!result.slang_ok) {
      return 1;
    }

    return RunCommand(
        CommandContext{
            .args = &args,
            .artifacts = &result.artifacts,
            .mgr = mgr,
            .dpi_inputs = *dpi_inputs,
            .formatting = args.format ? lyra::driver::SourceFormatting::kOn
                                      : lyra::driver::SourceFormatting::kOff,
            .report = &report,
            .program_path = program_path});
  } catch (const lyra::InternalError& e) {
    fmt::print(stderr, "{}", lyra::diag::RenderInternalError(e.what()));
    return 2;
  } catch (const std::exception& e) {
    fmt::print(stderr, "lyra: error: {}\n", e.what());
    return 2;
  }
}
