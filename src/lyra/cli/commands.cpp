#include "lyra/cli/commands.hpp"

#include <filesystem>
#include <iostream>
#include <optional>
#include <string>
#include <utility>

#include <fmt/core.h>
#include <slang/ast/ASTSerializer.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/text/Json.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/cli/command_line.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/cpp_build.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/jit/executor.hpp"
#include "lyra/lir/dump.hpp"
#include "lyra/mir/dump.hpp"
#include "lyra/support/subprocess.hpp"

namespace lyra::cli {

namespace {

// Where the bundled runtime headers and archive live, relative to this
// executable. Reports and returns nullopt when they cannot be found, so a
// caller only has to leave.
auto ResolveRuntime(const CommandContext& ctx)
    -> std::optional<driver::RuntimeLocation> {
  auto loc_or = driver::ResolveRuntimeLocation(std::string(ctx.program_path));
  if (!loc_or) {
    (*ctx.report)(
        diag::Make(diag::DiagCode::kHostIoError, std::move(loc_or.error())));
    return std::nullopt;
  }
  return *std::move(loc_or);
}

// Resolved here rather than up front because `dump` must keep working on a
// machine with no C++ compiler installed: a missing compiler is fatal only to
// the commands that would invoke one.
auto ResolveHostBuild(const CommandContext& ctx)
    -> std::optional<driver::HostBuild> {
  auto cxx_or = support::FindOnPath(ctx.args->cxx);
  if (!cxx_or) {
    (*ctx.report)(
        diag::Make(diag::DiagCode::kHostIoError, std::move(cxx_or.error())));
    return std::nullopt;
  }
  return driver::HostBuild{
      .cxx = *std::move(cxx_or),
      .pch = ctx.args->pch,
      .optimization = ctx.args->optimization};
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
  fmt::print("{}", hir::DumpHir(ctx.artifacts->HirUnits()));
  return 0;
}

auto RunDumpMir(const CommandContext& ctx) -> int {
  for (const auto& unit : ctx.artifacts->MirUnits()) {
    fmt::print("{}", mir::DumpMir(unit));
  }
  fmt::print("{}", mir::DumpMir(ctx.artifacts->RootUnit()));
  return 0;
}

auto RunDumpLir(const CommandContext& ctx) -> int {
  for (const auto& unit : ctx.artifacts->LirUnits()) {
    fmt::print("{}", lir::DumpLir(unit));
  }
  fmt::print("{}", lir::DumpLir(ctx.artifacts->RootLirUnit()));
  return 0;
}

auto RunDumpLlvm(const CommandContext& ctx) -> int {
  const auto print = [&](const lir::CompilationUnit& unit) -> bool {
    auto emitted = backend::llvm_backend::EmitModule(unit);
    if (!emitted) {
      (*ctx.report)(std::move(emitted.error()), ctx.mgr);
      return false;
    }
    fmt::print("{}", emitted->Print());
    return true;
  };
  for (const auto& unit : ctx.artifacts->LirUnits()) {
    if (!print(unit)) {
      return 1;
    }
  }
  return print(ctx.artifacts->RootLirUnit()) ? 0 : 1;
}

// Writes the portable project `emit cpp` produces and `compile` then builds.
// Both need the same runtime and host build, so both get them from here and
// neither restates the assembly.
auto AssemblePortableProject(const CommandContext& ctx)
    -> std::optional<driver::HostBuild> {
  auto runtime = ResolveRuntime(ctx);
  if (!runtime) {
    return std::nullopt;
  }
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return std::nullopt;
  }
  auto assembled = driver::AssembleProject(
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
  auto built = driver::BuildProject(ctx.args->out_dir, *host, ctx.dpi_inputs);
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
  auto work_dir = support::MakeTempDir();
  if (!work_dir) {
    (*ctx.report)(
        diag::Make(diag::DiagCode::kHostIoError, std::move(work_dir.error())));
    return 1;
  }
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return 1;
  }
  auto exit_code = driver::RunInPlace(
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
  auto dir = support::MakeTempDir();
  if (!dir) {
    (*ctx.report)(
        diag::Make(diag::DiagCode::kHostIoError, std::move(dir.error())));
    return std::nullopt;
  }
  if (auto surface = driver::WriteDpiSurface(
          *runtime, ctx.artifacts->MirUnits(), ctx.artifacts->RootUnit(), *dir);
      !surface) {
    (*ctx.report)(std::move(surface.error()), ctx.mgr);
    return std::nullopt;
  }
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return std::nullopt;
  }
  auto built =
      driver::BuildDpiSharedLibrary(ctx.dpi_inputs, host->cxx, *dir, *dir);
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
  auto exit_code = jit::Execute(
      ctx.artifacts->LirUnits(), ctx.artifacts->UnitMetadata(),
      ctx.artifacts->RootLirUnit(), ctx.artifacts->RootMetadata(), *dpi_library,
      ctx.args->child_args);
  if (!exit_code) {
    (*ctx.report)(std::move(exit_code.error()), ctx.mgr);
    return 1;
  }
  return *exit_code;
}

auto RunBackend(const CommandContext& ctx) -> int {
  switch (ctx.args->backend) {
    case Backend::kCpp:
      return RunCppBackend(ctx);
    case Backend::kJit:
      return RunJitBackend(ctx);
    case Backend::kAot:
    case Backend::kLli:
      (*ctx.report)(diag::Make(
          diag::DiagCode::kHostBackendUnimplemented,
          "this execution backend is not yet implemented"));
      return 1;
  }
  return 1;
}

}  // namespace

// How far the compiler has to lower for a command to have what it reads.
// Exhaustive on purpose: a new command must state its own depth rather than
// inherit one silently.
auto LoweringDepth(const ParsedArgs& args) -> compiler::StopAfter {
  switch (args.cmd) {
    case CommandKind::kCheck:
    case CommandKind::kDumpAst:
      return compiler::StopAfter::kParse;
    case CommandKind::kDumpHir:
      return compiler::StopAfter::kHir;
    case CommandKind::kDumpLir:
    case CommandKind::kDumpLlvm:
      return compiler::StopAfter::kLir;
    case CommandKind::kRun:
      return args.backend == Backend::kCpp ? compiler::StopAfter::kMir
                                           : compiler::StopAfter::kLir;
    case CommandKind::kDumpMir:
    case CommandKind::kEmitCpp:
    case CommandKind::kCompile:
    case CommandKind::kCacheClear:
      return compiler::StopAfter::kMir;
  }
  return compiler::StopAfter::kMir;
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
  throw InternalError("cache clear reached the compiling dispatch");
}

}  // namespace lyra::cli
