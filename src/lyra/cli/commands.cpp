#include "lyra/cli/commands.hpp"

#include <filesystem>
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

  slang::ast::Compilation& compilation = *ctx.elaborated->compilation;
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

// The design every command below the front end drives from. Lowering to HIR is
// where the elaborated AST's last reader finishes, so it is taken here, once
// per run, and a whole design's worth of it stops being resident.
auto DesignOf(const CommandContext& ctx)
    -> std::optional<compiler::ElaboratedDesign> {
  return compiler::LowerToHir(
      std::move(ctx.elaborated->compilation), ctx.elaborated->source_mapper,
      compiler::LoweringPolicy{.assertions = ctx.args->assertions}, *ctx.sink);
}

// Writes the design's emitted C++ sources into `dir` and answers with what
// assembling the program around them still needs, which is what each unit
// stated of the foreign name space. Every command that produces a C++ project
// does exactly this first, whether the project is the one the user asked for or
// a temporary one about to be built and run.
auto WriteCppSources(
    const CommandContext& ctx, const std::filesystem::path& dir)
    -> std::optional<std::vector<dpi::AbiFragment>> {
  auto design = DesignOf(ctx);
  if (!design) {
    return std::nullopt;
  }
  driver::CppProjectSink sources(dir, ctx.formatting);
  auto lowered = compiler::LowerToSemantic(
      *design, ctx.elaborated->diag_sources, *ctx.sink,
      [&](compiler::SemanticUnit unit) -> diag::Result<void> {
        return sources.Take(unit.mir);
      });
  if (!lowered) {
    return std::nullopt;
  }
  if (auto written = sources.Finish(lowered->root); !written) {
    ctx.sink->Report(std::move(written.error()));
    return std::nullopt;
  }
  return sources.TakeDpiFragments();
}

auto RunDumpHir(const CommandContext& ctx) -> int {
  auto design = DesignOf(ctx);
  if (!design) {
    return 1;
  }
  for (hir::CompilationUnit& slot : design->hir.units) {
    const hir::CompilationUnit unit = std::move(slot);
    fmt::print("{}", hir::DumpHir(unit));
  }
  return 0;
}

auto RunDumpMir(const CommandContext& ctx) -> int {
  auto design = DesignOf(ctx);
  if (!design) {
    return 1;
  }
  auto lowered = compiler::LowerToSemantic(
      *design, ctx.elaborated->diag_sources, *ctx.sink,
      [](compiler::SemanticUnit unit) -> diag::Result<void> {
        fmt::print("{}", mir::DumpMir(unit.mir));
        return {};
      });
  if (!lowered) {
    return 1;
  }
  fmt::print("{}", mir::DumpMir(lowered->root));
  return 0;
}

auto RunDumpLir(const CommandContext& ctx) -> int {
  auto design = DesignOf(ctx);
  if (!design) {
    return 1;
  }
  auto lowered = compiler::LowerToExecutable(
      *design, ctx.elaborated->diag_sources, *ctx.sink,
      [](compiler::ExecutableUnit unit) -> diag::Result<void> {
        fmt::print("{}", lir::DumpLir(unit.body));
        return {};
      });
  if (!lowered) {
    return 1;
  }
  fmt::print("{}", lir::DumpLir(lowered->root.body));
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
  auto design = DesignOf(ctx);
  if (!design) {
    return 1;
  }
  auto lowered = compiler::LowerToExecutable(
      *design, ctx.elaborated->diag_sources, *ctx.sink,
      [&](compiler::ExecutableUnit unit) -> diag::Result<void> {
        return print(unit.body);
      });
  if (!lowered) {
    return 1;
  }
  if (auto printed = print(lowered->root.body); !printed) {
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
  auto fragments = WriteCppSources(ctx, ctx.args->out_dir);
  if (!fragments) {
    return false;
  }
  if (auto assembled = driver::AssembleProject(
          *runtime, *fragments, ctx.args->out_dir, host, ctx.dpi_inputs);
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
  auto fragments = WriteCppSources(ctx, *work_dir);
  if (!fragments) {
    return 1;
  }
  auto exit_code = driver::RunInPlace(
      *runtime, *fragments, *work_dir, *host, ctx.args->child_args,
      ctx.dpi_inputs);
  if (!exit_code) {
    ctx.sink->Report(std::move(exit_code.error()));
    return 1;
  }
  return *exit_code;
}

// The design's DPI-C sources, compiled to the objects its execution session
// links. The temp directory holds those objects and the ABI header the sources
// compile against. Reached only for a design that has foreign sources.
auto BuildJitDpiObjects(
    const CommandContext& ctx, std::span<const dpi::AbiFragment> fragments)
    -> std::optional<std::vector<std::filesystem::path>> {
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
  if (auto surface = driver::WriteDpiSurface(*runtime, fragments, *dir);
      !surface) {
    ctx.sink->Report(std::move(surface.error()));
    return std::nullopt;
  }
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return std::nullopt;
  }
  auto built = driver::CompileDpiObjects(ctx.dpi_inputs, host->cxx, *dir, *dir);
  if (!built) {
    ctx.sink->Report(std::move(built.error()));
    return std::nullopt;
  }
  return *std::move(built);
}

auto RunJitBackend(const CommandContext& ctx) -> int {
  // Every unit's body is loaded into one execution session before the design
  // runs, and that session is where the names they hold in common resolve. This
  // is the one path here that holds the design: what it holds is the executable
  // bodies, and the MIR each was lowered from is released as it goes.
  //
  // It reads each unit at both depths rather than only the executable one: the
  // session loads the body, and what the unit states of the foreign name space
  // is a fact of its semantic model, taken while that model is still here.
  auto design = DesignOf(ctx);
  if (!design) {
    return 1;
  }
  std::vector<compiler::ExecutableUnit> units;
  std::vector<dpi::AbiFragment> fragments;
  auto lowered = compiler::LowerToSemantic(
      *design, ctx.elaborated->diag_sources, *ctx.sink,
      [&](compiler::SemanticUnit unit) -> diag::Result<void> {
        dpi::CollectAbiFragment(unit.mir, fragments);
        auto executable = compiler::LowerUnitToExecutable(unit.mir);
        if (!executable) {
          return std::unexpected(std::move(executable.error()));
        }
        units.push_back(*std::move(executable));
        return {};
      });
  if (!lowered) {
    return 1;
  }
  auto root = compiler::LowerUnitToExecutable(lowered->root);
  if (!root) {
    ctx.sink->Report(std::move(root.error()));
    return 1;
  }
  // A design that declares no foreign source compiles nothing here and links
  // nothing there.
  std::vector<std::filesystem::path> dpi_objects;
  if (!ctx.dpi_inputs.empty()) {
    auto built = BuildJitDpiObjects(ctx, fragments);
    if (!built) {
      return 1;
    }
    dpi_objects = *std::move(built);
  }
  // The design-root unit's construct elaborates the whole design, building the
  // top-level units as its owned children, so the JIT runs the design once from
  // that one entry rather than per top.
  auto exit_code =
      jit::Execute(units, *root, dpi_objects, ctx.args->child_args);
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
  throw InternalError("run: the request names no execution backend");
}

}  // namespace

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
