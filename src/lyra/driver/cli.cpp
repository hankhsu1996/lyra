#include <algorithm>
#include <argparse/argparse.hpp>
#include <array>
#include <cstdint>
#include <cstdio>
#include <exception>
#include <expected>
#include <filesystem>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unistd.h>
#include <utility>
#include <vector>

#include <fmt/core.h>

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

enum class CommandKind {
  kDumpHir,
  kDumpMir,
  kDumpLir,
  kDumpLlvm,
  kEmitCpp,
  kCompile,
  kRun,
  kCacheClear,
};

// How `run` executes the design. The C++ backend emits a C++ project and builds
// it; the LLVM backends share one emitted module and differ only in how they
// run it (in-process ORC JIT, ahead-of-time native compile, or the `lli` tool).
enum class Backend : std::uint8_t { kCpp, kJit, kAot, kLli };

// Whether diagnostics carry ANSI colour. `kAuto` asks the terminal; the other
// two are the user overriding that answer in either direction.
enum class ColorPreference : std::uint8_t { kAuto, kAlways, kNever };

struct ParsedArgs {
  CommandKind cmd = CommandKind::kEmitCpp;
  bool no_project = false;
  ColorPreference color = ColorPreference::kAuto;
  bool format = false;
  bool no_pch = false;
  Backend backend = Backend::kCpp;
  std::string pch_cache_dir;
  // The host C++ compiler the C++ backend builds emitted code with: a program
  // name or path, never flags.
  std::string cxx;
  lyra::frontend::CompilationInput input;
  std::string out_dir;
  // LRM 21.6 plusarg pass-through: `+`-prefixed positional entries collected
  // from the trailing bucket at parse time. `run` forwards them verbatim to
  // the built program's argv; other subcommands never consult them.
  std::vector<std::string> plusargs;
  // LRM 35 DPI-C link inputs: native source files (`.c` / `.cpp`) providing the
  // foreign symbols an `import "DPI-C"` calls. Compiled and linked into the
  // built program alongside the emitted C++.
  std::vector<std::string> dpi_link_sources;
};

void AddCompilationFlags(argparse::ArgumentParser& cmd) {
  cmd.add_argument("--no-project")
      .help("operate in direct file mode (no lyra.toml lookup)")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("--no-color")
      .help("disable ANSI color in diagnostics")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("--color")
      .help("force ANSI color in diagnostics (override TTY detection)")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("--top")
      .help("top module name")
      .default_value(std::string{});
  cmd.add_argument("-I", "--include-directory")
      .help("add include search directory")
      .append();
  cmd.add_argument("-D", "--define-macro")
      .help("define preprocessor macro (NAME or NAME=VALUE)")
      .append();
  cmd.add_argument("-G")
      .help("override module parameter (NAME=VALUE)")
      .metavar("NAME=VALUE")
      .append();
  cmd.add_argument("--single-unit")
      .help("compile all files as a single compilation unit")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("--disable-assertions")
      .help(
          "skip assertion constructs during lowering instead of rejecting them")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("--format")
      .help("reformat the emitted C++ with clang-format (skipped if absent)")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("--dpi-link")
      .help("native source (.c/.cpp) providing DPI-C foreign symbols to link")
      .append();
  cmd.add_argument("files").help("SystemVerilog source files").remaining();
}

void BindCompilationFlags(
    const argparse::ArgumentParser& cmd, ParsedArgs& out) {
  out.no_project = cmd.get<bool>("--no-project");
  if (cmd.get<bool>("--no-color")) {
    out.color = ColorPreference::kNever;
  } else if (cmd.get<bool>("--color")) {
    out.color = ColorPreference::kAlways;
  }
  out.input.top = cmd.get<std::string>("--top");
  if (auto incs =
          cmd.present<std::vector<std::string>>("--include-directory")) {
    out.input.incdirs = std::move(*incs);
  }
  if (auto dpi = cmd.present<std::vector<std::string>>("--dpi-link")) {
    out.dpi_link_sources = std::move(*dpi);
  }
  if (auto defs = cmd.present<std::vector<std::string>>("--define-macro")) {
    out.input.defines = std::move(*defs);
  }
  if (auto ovr = cmd.present<std::vector<std::string>>("-G")) {
    out.input.param_overrides = std::move(*ovr);
  }
  out.input.single_unit = cmd.get<bool>("--single-unit");
  out.input.disable_assertions = cmd.get<bool>("--disable-assertions");
  out.format = cmd.get<bool>("--format");
  if (auto files = cmd.present<std::vector<std::string>>("files")) {
    // argparse collects `.sv` paths and `+`-prefixed plusargs in the same
    // remaining-bucket; sort them here so downstream stages see the natural
    // shape (source files vs runtime tokens).
    for (auto& f : *files) {
      if (!f.empty() && f.front() == '+') {
        out.plusargs.push_back(std::move(f));
      } else {
        out.input.files.push_back(std::move(f));
      }
    }
  }
}

// The spelling of each execution backend on the command line. `--backend`
// restricts the value to exactly these, so a name outside the table means the
// two lists have drifted apart rather than that a user typed something odd.
auto ParseBackend(std::string_view name) -> Backend {
  static constexpr std::array<std::pair<std::string_view, Backend>, 4> kNames =
      {{{"cpp", Backend::kCpp},
        {"jit", Backend::kJit},
        {"aot", Backend::kAot},
        {"lli", Backend::kLli}}};
  const auto* const it =
      std::ranges::find(kNames, name, &decltype(kNames)::value_type::first);
  if (it == kNames.end()) {
    throw lyra::InternalError(
        std::format("--backend accepted an unmapped value '{}'", name));
  }
  return it->second;
}

// How the host builds emitted C++. `--cxx` names a program, never a flag list:
// a compiler that needs configuration to be a conforming C++23 implementation
// is named through a wrapper script or a driver config file that supplies it.
// The precompiled header caches the parse of the runtime headers; `--no-pch`
// skips it for one invocation and `--pch-cache-dir` moves it, which is how a
// test shard keeps its cache inside its own scratch directory.
//
// Registration and binding sit together because nothing else keeps them in
// step: a flag added to one and forgotten in the other fails silently.
void AddHostBuildFlags(argparse::ArgumentParser& cmd) {
  cmd.add_argument("--cxx")
      .help(
          "host C++ compiler for the C++ backend: a path, or a name found on "
          "PATH (default: clang++)")
      .default_value(std::string("clang++"));
  cmd.add_argument("--no-pch")
      .help("disable the precompiled-header cache for this invocation")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("--pch-cache-dir")
      .help("override the PCH cache directory")
      .default_value(std::string{});
}

void BindHostBuildFlags(const argparse::ArgumentParser& cmd, ParsedArgs& out) {
  out.cxx = cmd.get<std::string>("--cxx");
  out.no_pch = cmd.get<bool>("--no-pch");
  out.pch_cache_dir = cmd.get<std::string>("--pch-cache-dir");
}

void AddOutDirFlag(argparse::ArgumentParser& cmd, const char* help) {
  cmd.add_argument("-o", "--out-dir").help(help).default_value(std::string{});
}

// Binds `--out-dir`, which every command that has it also requires. The error
// names the command's own usage, so the caller supplies its parser.
auto BindRequiredOutDir(
    const argparse::ArgumentParser& cmd, std::string_view command,
    ParsedArgs& out) -> std::optional<std::string> {
  out.out_dir = cmd.get<std::string>("--out-dir");
  if (out.out_dir.empty()) {
    return std::format("{} requires --out-dir\n{}", command, cmd.help().str());
  }
  return std::nullopt;
}

auto ParseArgs(int argc, char** argv)
    -> std::expected<ParsedArgs, std::string> {
  argparse::ArgumentParser program("lyra");

  argparse::ArgumentParser dump_cmd("dump");
  argparse::ArgumentParser dump_hir_cmd("hir");
  argparse::ArgumentParser dump_mir_cmd("mir");
  argparse::ArgumentParser dump_lir_cmd("lir");
  argparse::ArgumentParser dump_llvm_cmd("llvm");
  AddCompilationFlags(dump_hir_cmd);
  AddCompilationFlags(dump_mir_cmd);
  AddCompilationFlags(dump_lir_cmd);
  AddCompilationFlags(dump_llvm_cmd);
  dump_cmd.add_subparser(dump_hir_cmd);
  dump_cmd.add_subparser(dump_mir_cmd);
  dump_cmd.add_subparser(dump_lir_cmd);
  dump_cmd.add_subparser(dump_llvm_cmd);

  argparse::ArgumentParser emit_cmd("emit");
  argparse::ArgumentParser emit_cpp_cmd("cpp");
  AddCompilationFlags(emit_cpp_cmd);
  AddHostBuildFlags(emit_cpp_cmd);
  AddOutDirFlag(
      emit_cpp_cmd, "write the self-contained C++ project to this directory");
  emit_cmd.add_subparser(emit_cpp_cmd);

  argparse::ArgumentParser compile_cmd("compile");
  AddCompilationFlags(compile_cmd);
  AddHostBuildFlags(compile_cmd);
  AddOutDirFlag(
      compile_cmd, "write the self-contained project and built program here");

  argparse::ArgumentParser run_cmd("run");
  AddCompilationFlags(run_cmd);
  AddHostBuildFlags(run_cmd);
  run_cmd.add_argument("--backend")
      .help("execution backend: cpp (default), jit, aot, or lli")
      .default_value(std::string("cpp"))
      .choices("cpp", "jit", "aot", "lli");

  argparse::ArgumentParser cache_cmd("cache");
  argparse::ArgumentParser cache_clear_cmd("clear");
  cache_clear_cmd.add_description(
      "remove the active precompiled-header cache directory's contents");
  cache_cmd.add_subparser(cache_clear_cmd);

  program.add_subparser(dump_cmd);
  program.add_subparser(emit_cmd);
  program.add_subparser(compile_cmd);
  program.add_subparser(run_cmd);
  program.add_subparser(cache_cmd);

  try {
    program.parse_args(argc, argv);
  } catch (const std::exception& e) {
    return std::unexpected(
        std::format("{}\n{}", e.what(), program.help().str()));
  }

  ParsedArgs out;
  if (program.is_subcommand_used("dump")) {
    if (dump_cmd.is_subcommand_used("hir")) {
      out.cmd = CommandKind::kDumpHir;
      BindCompilationFlags(dump_hir_cmd, out);
    } else if (dump_cmd.is_subcommand_used("mir")) {
      out.cmd = CommandKind::kDumpMir;
      BindCompilationFlags(dump_mir_cmd, out);
    } else if (dump_cmd.is_subcommand_used("lir")) {
      out.cmd = CommandKind::kDumpLir;
      BindCompilationFlags(dump_lir_cmd, out);
    } else if (dump_cmd.is_subcommand_used("llvm")) {
      out.cmd = CommandKind::kDumpLlvm;
      BindCompilationFlags(dump_llvm_cmd, out);
    } else {
      return std::unexpected(
          std::format(
              "dump requires 'hir', 'mir', 'lir', or 'llvm'\n{}",
              dump_cmd.help().str()));
    }
  } else if (program.is_subcommand_used("emit")) {
    if (!emit_cmd.is_subcommand_used("cpp")) {
      return std::unexpected(
          std::format("emit requires 'cpp'\n{}", emit_cmd.help().str()));
    }
    out.cmd = CommandKind::kEmitCpp;
    BindCompilationFlags(emit_cpp_cmd, out);
    BindHostBuildFlags(emit_cpp_cmd, out);
    if (auto e = BindRequiredOutDir(emit_cpp_cmd, "emit cpp", out)) {
      return std::unexpected(*std::move(e));
    }
  } else if (program.is_subcommand_used("compile")) {
    out.cmd = CommandKind::kCompile;
    BindCompilationFlags(compile_cmd, out);
    BindHostBuildFlags(compile_cmd, out);
    if (auto e = BindRequiredOutDir(compile_cmd, "compile", out)) {
      return std::unexpected(*std::move(e));
    }
  } else if (program.is_subcommand_used("run")) {
    out.cmd = CommandKind::kRun;
    BindCompilationFlags(run_cmd, out);
    BindHostBuildFlags(run_cmd, out);
    out.backend = ParseBackend(run_cmd.get<std::string>("--backend"));
  } else if (program.is_subcommand_used("cache")) {
    if (cache_cmd.is_subcommand_used("clear")) {
      out.cmd = CommandKind::kCacheClear;
    } else {
      return std::unexpected(
          std::format("cache requires 'clear'\n{}", cache_cmd.help().str()));
    }
  } else {
    return std::unexpected(program.help().str());
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
      return ::isatty(::fileno(stderr)) != 0;
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

auto RunDumpHir(const CommandContext& ctx) -> int {
  fmt::print("{}", lyra::hir::DumpHir(*ctx.artifacts->hir_units));
  return 0;
}

auto RunDumpMir(const CommandContext& ctx) -> int {
  for (const auto& unit : *ctx.artifacts->mir_units) {
    fmt::print("{}", lyra::mir::DumpMir(unit));
  }
  fmt::print("{}", lyra::mir::DumpMir(*ctx.artifacts->root_unit));
  return 0;
}

auto RunDumpLir(const CommandContext& ctx) -> int {
  for (const auto& unit : *ctx.artifacts->lir_units) {
    fmt::print("{}", lyra::lir::DumpLir(unit));
  }
  fmt::print("{}", lyra::lir::DumpLir(*ctx.artifacts->root_lir_unit));
  return 0;
}

auto RunDumpLlvm(const CommandContext& ctx) -> int {
  for (const auto& unit : *ctx.artifacts->lir_units) {
    fmt::print("{}", lyra::backend::llvm_backend::EmitModule(unit).Print());
  }
  fmt::print(
      "{}",
      lyra::backend::llvm_backend::EmitModule(*ctx.artifacts->root_lir_unit)
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
      *runtime, *ctx.artifacts->mir_units, *ctx.artifacts->root_unit,
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
      *runtime, *ctx.artifacts->mir_units, *ctx.artifacts->root_unit, *work_dir,
      ctx.formatting, *host, ctx.args->plusargs, ctx.dpi_inputs);
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
          *runtime, *ctx.artifacts->mir_units, *ctx.artifacts->root_unit, *dir);
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
      *ctx.artifacts->lir_units, *ctx.artifacts->unit_metadata,
      *ctx.artifacts->root_lir_unit, *ctx.artifacts->root_metadata,
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

}  // namespace

auto main(int argc, char** argv) -> int {
  try {
    const std::span<char* const> raw_args(argv, static_cast<std::size_t>(argc));
    const std::string program_path =
        raw_args.empty() ? std::string{} : std::string(raw_args.front());
    auto parsed = ParseArgs(argc, argv);
    const bool use_color =
        UseColor(parsed.has_value() ? parsed->color : ColorPreference::kAuto);
    const Reporter report{lyra::diag::RenderOptions{
        .use_color = use_color, .show_source_snippet = true}};

    if (!parsed) {
      report(
          lyra::diag::Make(
              lyra::diag::DiagCode::kHostInvalidCliArgs, parsed.error()));
      return 1;
    }
    const auto& args = *parsed;

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
    if (args.input.files.empty()) {
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
    auto result =
        lyra::compiler::Compile(args.input, sink, LoweringDepth(args));

    // `run` executes the simulation; its stdout/stderr are the simulation's
    // own, so compile-phase warnings must not bleed into them. Surface slang
    // diagnostics for run only when they carry errors (which abort below);
    // other commands always show them. Use `compile`/`dump` to see warnings.
    const bool suppress_compile_warnings =
        args.cmd == CommandKind::kRun && result.slang_ok && !sink.HasErrors();
    if (result.artifacts.parse && !suppress_compile_warnings) {
      std::string slang_text;
      lyra::frontend::RenderSlangDiagnostics(
          *result.artifacts.parse, use_color, slang_text);
      if (!slang_text.empty()) {
        fmt::print(stderr, "{}", slang_text);
      }
    }

    const lyra::diag::SourceManager* mgr =
        result.artifacts.parse ? &result.artifacts.parse->diag_sources
                               : nullptr;
    if (sink.HasErrors()) {
      fmt::print(
          stderr, "{}",
          lyra::diag::RenderDiagnostics(
              sink, mgr,
              lyra::diag::RenderOptions{
                  .use_color = use_color, .show_source_snippet = true}));
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
