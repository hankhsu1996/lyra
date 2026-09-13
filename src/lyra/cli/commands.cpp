#include "lyra/cli/commands.hpp"

#include <filesystem>
#include <format>
#include <iostream>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <fmt/core.h>
#include <slang/ast/ASTSerializer.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/text/Json.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/cli/command_line.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/compiler/unit_program_record.hpp"
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
// executable.
auto ResolveRuntime(const CommandContext& ctx)
    -> std::optional<driver::RuntimeLocation> {
  auto loc_or = driver::ResolveRuntimeLocation(std::string(ctx.program_path));
  if (!loc_or) {
    ctx.sink->Report(std::move(loc_or.error()));
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
    ctx.sink->Report(
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

// A product of the depth the command drove to. Absent here is a driver bug:
// which optionals are filled follows from that depth and from nothing else.
template <typename T>
auto Required(const std::optional<T>& product, std::string_view stage)
    -> const T& {
  if (!product.has_value()) {
    throw InternalError(std::format("cli: this run produced no {}", stage));
  }
  return *product;
}

// Drives everything below HIR to the depth this command reads. What a command
// does with each unit as it arrives is the only part that varies, so it is the
// only part a command states.
//
// Lowering reports every gap it meets rather than stopping at the first, so
// there is no one diagnostic to hand back and the account is read from the
// sink.
template <typename Consume>
auto DriveDesign(
    const CommandContext& ctx, compiler::StopAfter depth, Consume consume)
    -> std::optional<compiler::LoweredDesign> {
  compiler::ElaboratedDesign& design = ctx.artifacts->DesignToLower();
  return compiler::LowerDesign(
      design.hir, design.tops, depth, *ctx.mgr, *ctx.sink, std::move(consume));
}

// Writes the design's emitted C++ sources into `dir` and answers with what
// assembling the program around them still needs. Every command that produces
// a C++ project does exactly this first, whether the project is the one the
// user asked for or a temporary one about to be built and run.
auto WriteCppSources(
    const CommandContext& ctx, const std::filesystem::path& dir)
    -> std::optional<std::vector<compiler::UnitProgramRecord>> {
  driver::CppProjectSink sources(dir, ctx.formatting);
  auto lowered = DriveDesign(
      ctx, compiler::StopAfter::kMir,
      [&](compiler::UnitArtifacts unit) -> diag::Result<void> {
        return sources.Take(unit.mir, unit.program_record);
      });
  if (!lowered) {
    return std::nullopt;
  }
  if (auto written = sources.Finish(lowered->root.mir, lowered->records);
      !written) {
    ctx.sink->Report(std::move(written.error()));
    return std::nullopt;
  }
  return std::move(lowered->records);
}

auto RunDumpHir(const CommandContext& ctx) -> int {
  for (hir::CompilationUnit& slot : ctx.artifacts->DesignToLower().hir.units) {
    const hir::CompilationUnit unit = std::move(slot);
    fmt::print("{}", hir::DumpHir(unit));
  }
  return 0;
}

auto RunDumpMir(const CommandContext& ctx) -> int {
  auto lowered = DriveDesign(
      ctx, compiler::StopAfter::kMir,
      [](compiler::UnitArtifacts unit) -> diag::Result<void> {
        fmt::print("{}", mir::DumpMir(unit.mir));
        return {};
      });
  if (!lowered) {
    return 1;
  }
  fmt::print("{}", mir::DumpMir(lowered->root.mir));
  return 0;
}

auto RunDumpLir(const CommandContext& ctx) -> int {
  auto lowered = DriveDesign(
      ctx, compiler::StopAfter::kLir,
      [](compiler::UnitArtifacts unit) -> diag::Result<void> {
        // A namespace has no executable body, so it reaches here with none.
        if (unit.lir.has_value()) {
          fmt::print("{}", lir::DumpLir(*unit.lir));
        }
        return {};
      });
  if (!lowered) {
    return 1;
  }
  fmt::print("{}", lir::DumpLir(Required(lowered->root.lir, "LIR")));
  return 0;
}

auto RunDumpLlvm(const CommandContext& ctx) -> int {
  const auto print =
      [](const lir::CompilationUnit& unit) -> diag::Result<void> {
    auto emitted = backend::llvm_backend::EmitModule(unit);
    if (!emitted) {
      return std::unexpected(std::move(emitted.error()));
    }
    fmt::print("{}", emitted->Print());
    return {};
  };
  auto lowered = DriveDesign(
      ctx, compiler::StopAfter::kLir,
      [&](compiler::UnitArtifacts unit) -> diag::Result<void> {
        if (!unit.lir.has_value()) {
          return {};
        }
        return print(*unit.lir);
      });
  if (!lowered) {
    return 1;
  }
  if (auto printed = print(Required(lowered->root.lir, "LIR")); !printed) {
    ctx.sink->Report(std::move(printed.error()));
    return 1;
  }
  return 0;
}

// Writes the portable project `emit cpp` produces and `compile` then builds,
// so neither of them restates the assembly. The compiler is baked into the
// project's own build recipe, which is why a command that never builds
// anything still has to name one.
auto AssemblePortableProject(
    const CommandContext& ctx, const driver::HostBuild& host) -> bool {
  auto runtime = ResolveRuntime(ctx);
  if (!runtime) {
    return false;
  }
  auto records = WriteCppSources(ctx, ctx.args->out_dir);
  if (!records) {
    return false;
  }
  if (auto assembled = driver::AssembleProject(
          *runtime, *records, ctx.args->out_dir, host, ctx.dpi_inputs);
      !assembled) {
    ctx.sink->Report(std::move(assembled.error()));
    return false;
  }
  return true;
}

auto RunEmitCpp(const CommandContext& ctx) -> int {
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return 1;
  }
  if (!AssemblePortableProject(ctx, *host)) {
    return 1;
  }
  fmt::print("emitted: {}\n", ctx.args->out_dir);
  return 0;
}

auto RunCompile(const CommandContext& ctx) -> int {
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return 1;
  }
  if (!AssemblePortableProject(ctx, *host)) {
    return 1;
  }
  auto built = driver::BuildProject(ctx.args->out_dir, *host, ctx.dpi_inputs);
  if (!built) {
    ctx.sink->Report(std::move(built.error()));
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
    ctx.sink->Report(
        diag::Make(diag::DiagCode::kHostIoError, std::move(work_dir.error())));
    return 1;
  }
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return 1;
  }
  auto records = WriteCppSources(ctx, *work_dir);
  if (!records) {
    return 1;
  }
  auto exit_code = driver::RunInPlace(
      *runtime, *records, *work_dir, *host, ctx.args->child_args,
      ctx.dpi_inputs);
  if (!exit_code) {
    ctx.sink->Report(std::move(exit_code.error()));
    return 1;
  }
  return *exit_code;
}

// A JIT image has no link step, so the design's DPI-C sources are compiled
// into a library the execution session resolves the imports' foreign symbols
// from. The temp directory holds that library and the ABI header the sources
// compile against. Reached only for a design that has foreign sources.
auto BuildJitDpiLibrary(
    const CommandContext& ctx,
    std::span<const compiler::UnitProgramRecord> records)
    -> std::optional<std::filesystem::path> {
  auto runtime = ResolveRuntime(ctx);
  if (!runtime) {
    return std::nullopt;
  }
  auto dir = support::MakeTempDir();
  if (!dir) {
    ctx.sink->Report(
        diag::Make(diag::DiagCode::kHostIoError, std::move(dir.error())));
    return std::nullopt;
  }
  if (auto surface = driver::WriteDpiSurface(*runtime, records, *dir);
      !surface) {
    ctx.sink->Report(std::move(surface.error()));
    return std::nullopt;
  }
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return std::nullopt;
  }
  auto built =
      driver::BuildDpiSharedLibrary(ctx.dpi_inputs, host->cxx, *dir, *dir);
  if (!built) {
    ctx.sink->Report(std::move(built.error()));
    return std::nullopt;
  }
  return *std::move(built);
}

auto RunJitBackend(const CommandContext& ctx) -> int {
  // An in-process image has no link step, so every unit's body is loaded into
  // one execution session before the design runs. That is the one path here
  // that holds the design: what it holds is the executable bodies, and the MIR
  // each was lowered from is released as it goes.
  std::vector<lir::CompilationUnit> bodies;
  std::vector<compiler::ElaboratedUnitMetadata> definitions;
  auto lowered = DriveDesign(
      ctx, compiler::StopAfter::kLir,
      [&](compiler::UnitArtifacts unit) -> diag::Result<void> {
        // A namespace has no executable body, so what a session loads is the
        // units that have one.
        if (unit.lir.has_value()) {
          bodies.push_back(*std::move(unit.lir));
          definitions.push_back(Required(unit.metadata, "unit metadata"));
        }
        return {};
      });
  if (!lowered) {
    return 1;
  }
  // A design that declares no foreign source needs no library.
  std::optional<std::filesystem::path> dpi_library;
  if (!ctx.dpi_inputs.empty()) {
    dpi_library = BuildJitDpiLibrary(ctx, lowered->records);
    if (!dpi_library) {
      return 1;
    }
  }
  // The design-root unit's construct elaborates the whole design, building the
  // top-level units as its owned children, so the JIT runs the design once from
  // that one entry rather than per top.
  auto exit_code = jit::Execute(
      bodies, definitions, Required(lowered->root.lir, "LIR"),
      Required(lowered->root.metadata, "unit metadata"), dpi_library,
      ctx.args->child_args);
  if (!exit_code) {
    ctx.sink->Report(std::move(exit_code.error()));
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
      ctx.sink->Report(
          diag::Make(
              diag::DiagCode::kHostBackendUnimplemented,
              "this execution backend is not yet implemented"));
      return 1;
  }
  return 1;
}

}  // namespace

// How far the front end has to run for a command to have what it drives from.
// Everything below HIR is the command's own to drive, so the only question
// here is whether the request reads the elaborated design at all. Exhaustive
// on purpose: a new command must state its own answer rather than inherit one
// silently.
auto FrontEndDepth(const ParsedArgs& args) -> compiler::StopAfter {
  switch (args.cmd) {
    case CommandKind::kCheck:
    case CommandKind::kDumpAst:
      return compiler::StopAfter::kParse;
    case CommandKind::kDumpHir:
    case CommandKind::kDumpMir:
    case CommandKind::kDumpLir:
    case CommandKind::kDumpLlvm:
    case CommandKind::kRun:
    case CommandKind::kEmitCpp:
    case CommandKind::kCompile:
    case CommandKind::kCacheClear:
      return compiler::StopAfter::kHir;
  }
  return compiler::StopAfter::kHir;
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
