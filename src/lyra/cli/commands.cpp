#include "lyra/cli/commands.hpp"

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <exception>
#include <expected>
#include <filesystem>
#include <functional>
#include <iostream>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include <fmt/core.h>
#include <slang/ast/ASTSerializer.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/driver/Driver.h>
#include <slang/text/Json.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/backend/llvm/link_time_optimization.hpp"
#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/cli/command_line.hpp"
#include "lyra/cli/manifest.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/driver/action_cache.hpp"
#include "lyra/driver/claimed_file.hpp"
#include "lyra/driver/cpp_build.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/driver/scratch_directory.hpp"
#include "lyra/driver/subprocess.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/dump.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/dump.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/program/program_sink.hpp"
#include "lyra/status/status.hpp"
#include "lyra/support/bodies_read.hpp"
#include "lyra/support/statistics.hpp"

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

// The compiler named with `--cxx`, or the one a build takes when none is
// named.
auto ResolveCompiler(const CommandContext& ctx)
    -> std::optional<std::filesystem::path> {
  if (ctx.args->cxx) {
    auto named = driver::FindOnPath(*ctx.args->cxx);
    if (!named) {
      ctx.sink->Report(
          diag::Make(diag::DiagCode::kHostIoError, std::move(named.error())));
      return std::nullopt;
    }
    return *std::move(named);
  }
  if (auto found = driver::FindUsualCompiler()) {
    return found;
  }
  ctx.sink->Report(
      diag::Make(
          diag::DiagCode::kHostIoError,
          "no C++ compiler is found, since neither 'clang++' nor 'c++' is on "
          "PATH")
          .WithNote("name one with '--cxx'"));
  return std::nullopt;
}

// What the host compiler checks what it compiles for. On the C++ backend it
// compiles the design and the runtime, so the program's sanitizer is its own;
// the execution backend instruments the design's code itself and links the
// sanitizer's runtime of its own release, which the host's would not pair with.
auto HostSanitizer(const CommandContext& ctx) -> support::Sanitizer {
  switch (ctx.args->backend) {
    case Backend::kCpp:
      return ctx.args->sanitizer;
    case Backend::kLlvm:
      return support::Sanitizer::kNone;
  }
  std::unreachable();
}

// Resolved here rather than up front because `dump` must keep working on a
// machine with no C++ compiler installed: a missing compiler is fatal only to
// the commands that would invoke one.
auto ResolveHostBuild(const CommandContext& ctx)
    -> std::optional<driver::HostBuild> {
  auto cxx = ResolveCompiler(ctx);
  if (!cxx) {
    return std::nullopt;
  }
  return driver::HostBuild{
      .cxx = *std::move(cxx),
      .pch = ctx.args->pch,
      .optimization = ctx.args->optimization,
      .link_time = ctx.args->link_time,
      .sanitizer = HostSanitizer(ctx),
      .compile_width = ctx.args->compile_width};
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

// The design every command below the front end drives from. The elaborated
// AST is handed to its units here, once per run, and every unit reads it until
// the last of them has been lowered.
auto DesignOf(const CommandContext& ctx)
    -> std::optional<compiler::ElaboratedDesign> {
  const profiling::StageScope stage("declare units");
  const status::Phase phase("Elaborating");
  return compiler::DeclareUnits(
      std::move(ctx.elaborated->compilation),
      compiler::LoweringPolicy{
          .assertions = ctx.args->assertions,
          .bodies_read = support::BodiesRead::kOnePerName},
      *ctx.sink);
}

// Writes the design's emitted C++ sources into `dir` and answers with what
// assembling and building the program around them still needs. Every command
// that produces a C++ project does exactly this first, whether the project is
// the one the user asked for or one a build compiles and then drops.
auto WriteCppSources(
    const CommandContext& ctx, const std::filesystem::path& dir)
    -> std::optional<driver::EmittedCppSources> {
  auto design = DesignOf(ctx);
  if (!design) {
    return std::nullopt;
  }
  const status::Phase phase("Generating C++");
  driver::CppProjectSink sources(dir, ctx.args->formatting, *ctx.sink);
  auto lowered = compiler::LowerToSemantic(
      *design, ctx.elaborated->diag_sources, ctx.args->mir_passes, *ctx.sink,
      ctx.args->compile_width,
      [&](mir::CompilationUnit unit) { return sources.Write(unit); },
      [&](driver::EmittedUnit unit) { sources.Collect(std::move(unit)); });
  if (!lowered) {
    return std::nullopt;
  }
  if (auto written =
          [&] {
            const profiling::StageScope stage("emit C++ root");
            return sources.Finish(lowered->root);
          }();
      !written) {
    ctx.sink->Report(std::move(written.error()));
    return std::nullopt;
  }
  // A unit refused somewhere leaves a project short of the design, which is no
  // program to build.
  if (ctx.sink->HasErrors()) {
    return std::nullopt;
  }
  return sources.TakeSources();
}

auto RunDumpHir(const CommandContext& ctx) -> int {
  auto design = DesignOf(ctx);
  if (!design) {
    return 1;
  }
  compiler::LowerToHir(
      *design, *ctx.sink, ctx.args->compile_width,
      [](const hir::CompilationUnit& unit) -> diag::Result<std::string> {
        return hir::DumpHir(unit);
      },
      [](const std::string& text) { fmt::print("{}", text); });
  return ctx.sink->HasErrors() ? 1 : 0;
}

auto RunDumpMir(const CommandContext& ctx) -> int {
  auto design = DesignOf(ctx);
  if (!design) {
    return 1;
  }
  auto lowered = compiler::LowerToSemantic(
      *design, ctx.elaborated->diag_sources, ctx.args->mir_passes, *ctx.sink,
      ctx.args->compile_width,
      [](mir::CompilationUnit unit) -> diag::Result<std::string> {
        return mir::DumpMir(unit);
      },
      [](const std::string& text) { fmt::print("{}", text); });
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
      *design, ctx.elaborated->diag_sources, ctx.args->mir_passes, *ctx.sink,
      ctx.args->compile_width,
      [](const lir::CompilationUnit& unit) -> diag::Result<std::string> {
        return lir::DumpLir(unit);
      },
      [](const std::string& text) { fmt::print("{}", text); });
  if (!lowered) {
    return 1;
  }
  fmt::print("{}", lir::DumpLir(lowered->root));
  return 0;
}

auto RunDumpLlvm(const CommandContext& ctx) -> int {
  const auto text =
      [](const lir::CompilationUnit& unit) -> diag::Result<std::string> {
    auto emitted = backend::llvm_backend::EmitModule(unit);
    if (!emitted) {
      return std::unexpected(std::move(emitted.error()));
    }
    return emitted->Print();
  };
  const auto print = [](const std::string& module) {
    fmt::print("{}", module);
  };
  auto design = DesignOf(ctx);
  if (!design) {
    return 1;
  }
  auto lowered = compiler::LowerToExecutable(
      *design, ctx.elaborated->diag_sources, ctx.args->mir_passes, *ctx.sink,
      ctx.args->compile_width, text, print);
  if (!lowered) {
    return 1;
  }
  auto root = text(lowered->root);
  if (!root) {
    ctx.sink->Report(std::move(root.error()));
    return 1;
  }
  print(*root);
  fmt::print(
      "{}", backend::llvm_backend::EmitProgramEntry(lowered->root).Print());
  return 0;
}

// Writes the portable project `emit cpp` produces. The compiler is baked into
// the project's own build recipe, which is why a command that never builds
// anything still has to name one.
auto RunEmitCpp(const CommandContext& ctx) -> int {
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return 1;
  }
  auto runtime = ResolveRuntime(ctx);
  if (!runtime) {
    return 1;
  }
  const std::filesystem::path& dir = *ctx.args->out;
  auto sources = WriteCppSources(ctx, dir);
  if (!sources) {
    return 1;
  }
  if (auto assembled =
          [&] {
            const profiling::StageScope stage("assemble project");
            const status::Phase phase("Copying runtime");
            return driver::AssembleProject(
                *runtime, *sources, dir, *host, ctx.dpi_inputs);
          }();
      !assembled) {
    ctx.sink->Report(std::move(assembled.error()));
    return 1;
  }
  status::Finished();
  fmt::print("emitted: {}\n", dir.string());
  return 0;
}

// A directory of this invocation's own, which a program is built in and which
// goes when the command is done with it.
auto ScratchDir(const CommandContext& ctx)
    -> std::optional<driver::ScratchDirectory> {
  auto dir = driver::ScratchDirectory::Create();
  if (!dir) {
    ctx.sink->Report(
        diag::Make(diag::DiagCode::kHostIoError, std::move(dir.error())));
    return std::nullopt;
  }
  return *std::move(dir);
}

// The design's DPI-C sources, compiled beside the ABI header they compile
// against into `dir`, as the objects the program links. A design declaring no
// foreign source writes the header and compiles nothing.
auto BuildDpiObjects(
    const CommandContext& ctx, std::span<const dpi::AbiFragment> fragments,
    const driver::RuntimeLocation& runtime, const driver::HostBuild& host,
    const std::filesystem::path& dir)
    -> std::optional<std::vector<std::filesystem::path>> {
  const profiling::StageScope stage("foreign sources");
  if (auto surface = driver::WriteDpiSurface(runtime, fragments, dir);
      !surface) {
    ctx.sink->Report(std::move(surface.error()));
    return std::nullopt;
  }
  auto built = driver::CompileDpiObjects(
      ctx.dpi_inputs, host.cxx, host.optimization, host.sanitizer,
      host.compile_width, dir,
      dir / driver::kObjectDir / driver::kDpiSourceDir);
  if (!built) {
    ctx.sink->Report(std::move(built.error()));
    return std::nullopt;
  }
  return *std::move(built);
}

// How a design's program is made from its objects: the link, an action reading
// the bytes of the objects it links, the runtime they link against, the driver
// that links them, and the foreign objects beside them -- so an object that
// comes out the same however it was reached leaves the program kept -- and the
// link itself, which runs only when the action has no kept result. How many
// units were built at once changes none of the objects, so it is in no action.
struct ProgramRecipe {
  driver::Action action;
  driver::ActionStep build;
};

auto LinkRecipe(
    std::span<const driver::ActionResult> objects,
    std::span<const std::filesystem::path> foreign,
    const driver::LinkedRuntime& runtime, const driver::HostBuild& host)
    -> ProgramRecipe {
  driver::Action action("link");
  std::vector<std::filesystem::path> linked;
  linked.reserve(objects.size() + foreign.size() + runtime.files.size());
  for (const driver::ActionResult& object : objects) {
    action.InputDigest("object", object.digest);
    linked.push_back(object.path);
  }
  for (const std::filesystem::path& file : runtime.files) {
    action.InputFile("runtime", file);
  }
  for (const std::string& flag : runtime.flags) {
    action.Input("runtime flag", flag);
  }
  action.InputExecutable("linker", host.cxx);
  for (const std::filesystem::path& object : foreign) {
    action.InputFile("foreign object", object);
    linked.push_back(object);
  }
  linked.insert(linked.end(), runtime.files.begin(), runtime.files.end());
  return ProgramRecipe{
      .action = std::move(action),
      .build = [linked = std::move(linked), flags = runtime.flags,
                cxx = host.cxx](const std::filesystem::path& program) {
        const profiling::StageScope stage("link");
        return driver::LinkProgram(linked, flags, program, cxx);
      }};
}

// What a C++ backend program links of the runtime: the prebuilt library, or,
// where its compiler checks it for a sanitizer, the runtime compiled from its
// sources by that compiler, which links its own sanitizer's runtime.
auto CppRuntime(
    const driver::RuntimeLocation& runtime, const driver::HostBuild& host,
    driver::ActionCache& actions) -> diag::Result<driver::LinkedRuntime> {
  switch (host.sanitizer) {
    case support::Sanitizer::kNone:
      return driver::LinkedRuntime{.files = {runtime.lib}, .flags = {}};
    case support::Sanitizer::kAddress:
      break;
  }
  auto objects =
      driver::CompileRuntimeSources(runtime, host, std::nullopt, actions);
  if (!objects) {
    return std::unexpected(std::move(objects.error()));
  }
  driver::LinkedRuntime linked{
      .files = {}, .flags = driver::SanitizerLinkFlags(host.sanitizer)};
  for (const driver::ActionResult& object : *objects) {
    linked.files.push_back(object.path);
  }
  return linked;
}

// Where a link optimizing the whole program keeps what it compiled per module,
// in `layout`. A build told to keep nothing reads nothing kept either.
auto LinkTimeCacheDir(const CommandContext& ctx, driver::LinkTimeCache layout)
    -> std::optional<std::filesystem::path> {
  if (!ctx.args->store || ctx.args->rebuild) {
    return std::nullopt;
  }
  return driver::LinkTimeCacheIn(*ctx.args->store, layout);
}

// The C++ backend's program built the plain way: each emitted file compiled
// at the build's level, and linked with the runtime the build checks for.
auto PlainCppRecipe(
    std::span<const driver::TranslationUnit> translation_units,
    std::span<const std::filesystem::path> foreign,
    const std::filesystem::path& project,
    const driver::RuntimeLocation& runtime, const driver::HostBuild& host,
    driver::ActionCache& actions) -> diag::Result<ProgramRecipe> {
  auto objects = [&] {
    const profiling::StageScope stage("host compile");
    return driver::CompileTranslationUnits(
        project, translation_units, runtime, host, std::nullopt, actions);
  }();
  if (!objects) {
    return std::unexpected(std::move(objects.error()));
  }
  auto linked = CppRuntime(runtime, host, actions);
  if (!linked) {
    return std::unexpected(std::move(linked.error()));
  }
  return LinkRecipe(*objects, foreign, *linked, host);
}

// The C++ backend's program optimized whole by its host compiler at the link,
// under `form`, which a probe has accepted: the design's files and the
// runtime's own sources compiled for that, and linked so.
auto LinkTimeOptimizedCppRecipe(
    const CommandContext& ctx, const driver::LinkTimeForm& form,
    std::span<const driver::TranslationUnit> translation_units,
    std::span<const std::filesystem::path> foreign,
    const std::filesystem::path& project,
    const driver::RuntimeLocation& runtime, const driver::HostBuild& host,
    driver::ActionCache& actions) -> diag::Result<ProgramRecipe> {
  const profiling::StageScope stage("host compile");
  auto design = driver::CompileTranslationUnits(
      project, translation_units, runtime, host, form.compile, actions);
  if (!design) {
    return std::unexpected(std::move(design.error()));
  }
  auto runtime_objects =
      driver::CompileRuntimeSources(runtime, host, form.compile, actions);
  if (!runtime_objects) {
    return std::unexpected(std::move(runtime_objects.error()));
  }
  driver::Action action("link, optimizing the whole program");
  std::vector<std::filesystem::path> linked;
  for (const driver::ActionResult& object : *design) {
    action.InputDigest("object", object.digest);
    linked.push_back(object.path);
  }
  for (const driver::ActionResult& object : *runtime_objects) {
    action.InputDigest("runtime object", object.digest);
    linked.push_back(object.path);
  }
  action.InputExecutable("linker", host.cxx);
  action.Input("flag", form.compile);
  action.Input("level", driver::OptimizationFlag(host.optimization));
  for (const std::filesystem::path& object : foreign) {
    action.InputFile("foreign object", object);
    linked.push_back(object);
  }
  std::vector<std::string> link_time = driver::LinkFlagsFor(
      form, host.optimization,
      form.cache ? LinkTimeCacheDir(ctx, form.cache->layout) : std::nullopt);
  for (std::string& flag : driver::SanitizerLinkFlags(host.sanitizer)) {
    action.Input("runtime flag", flag);
    link_time.push_back(std::move(flag));
  }
  return ProgramRecipe{
      .action = std::move(action),
      .build = [linked = std::move(linked), link_time = std::move(link_time),
                cxx = host.cxx](const std::filesystem::path& program) {
        const profiling::StageScope stage("link");
        return driver::LinkProgram(linked, link_time, program, cxx);
      }};
}

// The C++ backend's program. Each emitted file is compiled by an action
// reading what the host compiler reads of the project, and the program is
// linked from what they produced -- with the prebuilt runtime, or the runtime
// compiled beside it where the build checks the program for a sanitizer, or
// where the build optimizes at the link, optimized whole with the runtime
// compiled beside it where the host compiler accepts a way to.
auto CppProgramRecipe(
    const CommandContext& ctx, const driver::HostBuild& host,
    const driver::RuntimeLocation& runtime,
    const std::filesystem::path& scratch, driver::ActionCache& actions)
    -> std::optional<ProgramRecipe> {
  const std::filesystem::path project = scratch / "project";
  auto sources = WriteCppSources(ctx, project);
  if (!sources) {
    return std::nullopt;
  }
  auto foreign =
      BuildDpiObjects(ctx, sources->dpi_fragments, runtime, host, project);
  if (!foreign) {
    return std::nullopt;
  }
  auto recipe = [&]() -> diag::Result<ProgramRecipe> {
    switch (host.link_time) {
      case driver::LinkTimeOptimization::kNone:
        break;
      case driver::LinkTimeOptimization::kThin:
        if (const auto form = driver::AcceptedLinkTimeForm(host, actions)) {
          return LinkTimeOptimizedCppRecipe(
              ctx, *form, sources->translation_units, *foreign, project,
              runtime, host, actions);
        }
        break;
    }
    return PlainCppRecipe(
        sources->translation_units, *foreign, project, runtime, host, actions);
  }();
  if (!recipe) {
    ctx.sink->Report(std::move(recipe.error()));
    return std::nullopt;
  }
  return *std::move(recipe);
}

// This compiler's own executable. The LLVM backend's code generator is part of
// it, so the objects that backend writes are a function of which build of it
// is running.
auto ThisCompiler(const CommandContext& ctx) -> std::filesystem::path {
  std::error_code ec;
  auto self = std::filesystem::read_symlink("/proc/self/exe", ec);
  return ec ? std::filesystem::path(ctx.program_path) : self;
}

// The execution backend's program where the build optimizes at the link: the
// design's modules and the runtime's bitcode optimized as one program at the
// build's level, then linked by the host with the foreign objects and whatever
// of the runtime carries no bitcode. The optimization is this compiler's own
// code generation, so the compiler is an input of the action as the host
// linker is.
auto LinkTimeOptimizedLlvmRecipe(
    const CommandContext& ctx, std::span<const driver::ActionResult> modules,
    std::span<const std::filesystem::path> foreign,
    const driver::RuntimeLocation& runtime, const driver::HostBuild& host)
    -> ProgramRecipe {
  driver::Action action("link, optimizing the whole program");
  std::vector<std::filesystem::path> module_paths;
  module_paths.reserve(modules.size());
  for (const driver::ActionResult& module : modules) {
    action.InputDigest("module", module.digest);
    module_paths.push_back(module.path);
  }
  driver::LinkedRuntime linked_runtime =
      driver::ExecutionBackendRuntime(runtime, ctx.args->sanitizer);
  const std::filesystem::path& runtime_lib =
      driver::ExecutionBackendLibrary(runtime, ctx.args->sanitizer);
  for (const std::filesystem::path& file : linked_runtime.files) {
    action.InputFile("runtime", file);
  }
  for (const std::string& flag : linked_runtime.flags) {
    action.Input("runtime flag", flag);
  }
  action.InputExecutable("linker", host.cxx);
  action.InputExecutable("code generator", ThisCompiler(ctx));
  action.Input("level", driver::OptimizationFlag(host.optimization));
  std::vector<std::filesystem::path> foreign_paths;
  foreign_paths.reserve(foreign.size());
  for (const std::filesystem::path& object : foreign) {
    action.InputFile("foreign object", object);
    foreign_paths.push_back(object);
  }
  std::optional<std::filesystem::path> cache =
      LinkTimeCacheDir(ctx, driver::LinkTimeCache::kLlvm);
  return ProgramRecipe{
      .action = std::move(action),
      .build = [module_paths = std::move(module_paths),
                foreign_paths = std::move(foreign_paths), runtime_lib,
                linked_runtime = std::move(linked_runtime), cxx = host.cxx,
                level = program::LlvmLevelOf(host.optimization),
                width = host.compile_width, cache = std::move(cache)](
                   const std::filesystem::path& program) -> diag::Result<void> {
        std::filesystem::path dir = program;
        dir += ".objects";
        auto optimized = [&] {
          const profiling::StageScope stage("link-time optimization");
          return backend::llvm_backend::OptimizeAtLink(
              {.modules = module_paths,
               .runtime_library = runtime_lib,
               .machine_code = foreign_paths},
              level, dir, width, cache);
        }();
        if (!optimized) {
          return std::unexpected(std::move(optimized.error()));
        }
        std::vector<std::filesystem::path> linked = *std::move(optimized);
        linked.insert(linked.end(), foreign_paths.begin(), foreign_paths.end());
        linked.insert(
            linked.end(), linked_runtime.files.begin(),
            linked_runtime.files.end());
        const profiling::StageScope stage("link");
        return driver::LinkProgram(linked, linked_runtime.flags, program, cxx);
      }};
}

// The LLVM backend's program. Each unit is taken all the way to its object as
// it is lowered -- a kept object taken in place of compiling one -- so no unit
// waits on another's, and every form a unit passed through is released as it
// goes.
auto LlvmProgramRecipe(
    const CommandContext& ctx, const driver::HostBuild& host,
    const driver::RuntimeLocation& runtime,
    const std::filesystem::path& scratch, driver::ActionCache& actions)
    -> std::optional<ProgramRecipe> {
  auto design = DesignOf(ctx);
  if (!design) {
    return std::nullopt;
  }
  const program::ObjectBuild objects{
      .optimization = ctx.args->optimization,
      .link_time = ctx.args->link_time,
      .sanitizer = ctx.args->sanitizer,
      .code_generator = ThisCompiler(ctx)};
  program::ProgramSink sink;
  const status::Phase phase("Compiling");
  auto lowered = compiler::LowerToSemantic(
      *design, ctx.elaborated->diag_sources, ctx.args->mir_passes, *ctx.sink,
      host.compile_width,
      [&objects, &actions](mir::CompilationUnit unit) {
        return program::BuildUnit(unit, objects, actions);
      },
      [&sink](program::BuiltUnit unit) { sink.Collect(std::move(unit)); });
  if (!lowered) {
    return std::nullopt;
  }
  auto built = [&] {
    const profiling::StageScope stage("generate root object");
    return std::move(sink).Finish(lowered->root, objects, actions);
  }();
  if (!built) {
    ctx.sink->Report(std::move(built.error()));
    return std::nullopt;
  }
  auto foreign =
      BuildDpiObjects(ctx, built->dpi_fragments, runtime, host, scratch);
  if (!foreign) {
    return std::nullopt;
  }
  switch (host.link_time) {
    case driver::LinkTimeOptimization::kNone:
      return LinkRecipe(
          built->objects, *foreign,
          driver::ExecutionBackendRuntime(runtime, ctx.args->sanitizer), host);
    case driver::LinkTimeOptimization::kThin:
      return LinkTimeOptimizedLlvmRecipe(
          ctx, built->objects, *foreign, runtime, host);
  }
  std::unreachable();
}

auto RecipeFor(
    const CommandContext& ctx, const driver::HostBuild& host,
    const driver::RuntimeLocation& runtime,
    const std::filesystem::path& scratch, driver::ActionCache& actions)
    -> std::optional<ProgramRecipe> {
  switch (ctx.args->backend) {
    case Backend::kCpp:
      return CppProgramRecipe(ctx, host, runtime, scratch, actions);
    case Backend::kLlvm:
      return LlvmProgramRecipe(ctx, host, runtime, scratch, actions);
  }
  throw InternalError("the request names no backend");
}

// Places the design's program at `destination`: a copy of the output of the
// action making it, kept or performed now.
auto PlaceProgram(
    const CommandContext& ctx, const driver::RuntimeLocation& runtime,
    const std::filesystem::path& scratch,
    const std::filesystem::path& destination) -> bool {
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return false;
  }
  driver::ActionCache actions(ctx.args->store, scratch, !ctx.args->rebuild);
  auto recipe = RecipeFor(ctx, *host, runtime, scratch, actions);
  if (!recipe) {
    return false;
  }
  auto built = actions.Execute(recipe->action, std::move(recipe->build));
  if (!built) {
    ctx.sink->Report(std::move(built.error()));
    return false;
  }
  auto copied = driver::CopyOut(built->path, destination);
  if (!copied) {
    ctx.sink->Report(std::move(copied.error()));
    return false;
  }
  return true;
}

// Where `build` writes the program when it was not told: the working
// directory, under the name of the library a declaration was read for, or the
// design's top when no declaration was read and it has one. Several tops with
// no declaration have no name to take, so the program has to be given one.
auto DefaultProgramPath(const CommandContext& ctx)
    -> std::optional<std::filesystem::path> {
  if (ctx.args->library_name) {
    return std::filesystem::path(*ctx.args->library_name);
  }
  const auto tops = ctx.elaborated->compilation->getRoot().topInstances;
  if (tops.size() == 1) {
    return std::filesystem::path(std::string(tops.front()->name));
  }
  ctx.sink->Report(
      diag::Make(
          diag::DiagCode::kHostInvalidCliArgs,
          "this design has {} tops and no library name, so the program has "
          "no name to take",
          tops.size())
          .WithNote("name the program with '-o'"));
  return std::nullopt;
}

// Builds the design's program into the place claimed for it. A program the
// request named is claimed before the design is read; one that takes the
// design's name is claimed here, which is the first moment that name is known.
auto RunBuild(
    const CommandContext& ctx, std::optional<driver::ClaimedFile> named)
    -> int {
  if (!named) {
    const auto destination = DefaultProgramPath(ctx);
    if (!destination) {
      return 1;
    }
    auto claimed = driver::ClaimedFile::Claim(*destination);
    if (!claimed) {
      ctx.sink->Report(std::move(claimed.error()));
      return 1;
    }
    named = *std::move(claimed);
  }
  auto runtime = ResolveRuntime(ctx);
  if (!runtime) {
    return 1;
  }
  auto scratch = ScratchDir(ctx);
  if (!scratch ||
      !PlaceProgram(ctx, *runtime, scratch->Path(), named->WorkingPath())) {
    return 1;
  }
  if (auto placed = named->Finish(); !placed) {
    ctx.sink->Report(std::move(placed.error()));
    return 1;
  }
  status::Finished();
  fmt::print("built: {}\n", named->Destination().string());
  return 0;
}

// What the program a run starts is told beside this process's environment. A
// program checked for the address sanitizer names the functions in its report
// only through a symbolizer its runtime starts, which it looks for where the
// environment says, so it is told the one shipped with its runtime, as a test
// runner tells the programs it starts. One the person running Lyra named
// stands, by that variable or among the sanitizer's options, which the runtime
// reads over it.
auto ProgramEnvironment(
    const CommandContext& ctx, const driver::RuntimeLocation& runtime)
    -> std::vector<driver::EnvironmentVariable> {
  constexpr const char* kSymbolizerVariable = "ASAN_SYMBOLIZER_PATH";
  switch (ctx.args->sanitizer) {
    case support::Sanitizer::kNone:
      return {};
    case support::Sanitizer::kAddress:
      break;
  }
  if (std::getenv(kSymbolizerVariable) != nullptr) {
    return {};
  }
  return {driver::EnvironmentVariable{
      .name = kSymbolizerVariable,
      .value = runtime.address_sanitizer.symbolizer.string()}};
}

// Runs a copy of the design's program that belongs to this run alone, so
// nothing another process does to what is kept can reach it.
auto RunProgram(const CommandContext& ctx) -> int {
  auto runtime = ResolveRuntime(ctx);
  if (!runtime) {
    return 1;
  }
  auto scratch = ScratchDir(ctx);
  if (!scratch) {
    return 1;
  }
  const std::filesystem::path program = scratch->Path() / "run";
  if (!PlaceProgram(ctx, *runtime, scratch->Path(), program)) {
    return 1;
  }
  status::Finished();
  auto exit_code = driver::RunProcessStreaming(
      program, ctx.args->simulation_args, ProgramEnvironment(ctx, *runtime));
  if (!exit_code) {
    ctx.sink->Report(
        diag::Make(diag::DiagCode::kHostIoError, std::move(exit_code.error())));
    return 1;
  }
  return *exit_code;
}

// Whether the compiler's warnings reach the terminal, the front end's and its
// own alike. A program that is run owns its streams, so what the compiler had
// to say about the source stays out of them unless it refused the source
// outright.
enum class CompilerWarnings : std::uint8_t { kShown, kWithheld };

// The request resolved against the design's declaration: everything that is
// known before the design is read.
struct Request {
  ParsedArgs args;
  std::vector<driver::DpiLinkInput> dpi_inputs;
};

// Finds the design the command line and its declaration describe and resolves
// the request against it, reporting whatever stops that. Nothing of the design
// is read yet, so what a command can refuse from the request alone it refuses
// after this and before the front end runs.
auto ResolveRequest(const Invocation& invocation) -> std::optional<Request> {
  const Reporter& report = *invocation.report;
  slang::driver::Driver& driver = *invocation.driver;

  auto declaration_or = ResolveDesignDeclaration(*invocation.options, driver);
  if (!declaration_or) {
    report(std::move(declaration_or.error()));
    return std::nullopt;
  }
  // The declaration is flattened once into the two things the rest of the run
  // reads: what was declared, to apply and re-read, and an absent search kept
  // for the no-input-files note below. That note is the only place absence
  // still speaks -- a named source means there are files, so no search reaches
  // that branch.
  const Declarations* declared = nullptr;
  std::optional<ManifestAbsent> absent;
  std::visit(
      Overloaded{
          [&](const Declarations& d) { declared = &d; },
          [&](const ManifestAbsent& a) { absent = a; }, [&](NoSearchNeeded) {}},
      *declaration_or);
  if (declared != nullptr) {
    if (auto applied = ApplyDeclarations(*declared, driver); !applied) {
      report(std::move(applied.error()));
      return std::nullopt;
    }
  }

  auto parsed = ResolveCliOptions(
      *invocation.options, declared, invocation.command,
      invocation.simulation_args);
  if (!parsed) {
    report(parsed.error());
    return std::nullopt;
  }

  if (!driver.sourceLoader.hasFiles()) {
    auto diagnostic =
        diag::Make(diag::DiagCode::kHostNoInputFiles, "no input files");
    if (absent) {
      diagnostic = std::move(diagnostic)
                       .WithNote(
                           "searched for 'lyra.toml' from '{}' up to '{}'",
                           absent->started.string(), absent->stopped.string());
    }
    // A declaration was in effect and still named nothing, which reads as no
    // declaration at all unless the message says which one applied -- and the
    // one that applied may be several directories above the caller.
    if (declared != nullptr) {
      diagnostic =
          std::move(diagnostic)
              .WithNote(
                  "library '{}' at '{}' declares no source files",
                  declared->root.library.name, declared->root.path.string());
    }
    report(std::move(diagnostic));
    return std::nullopt;
  }

  // Classified before compiling anything, so a mistyped path is reported
  // against the command line rather than after a full front end and lowering
  // pass.
  auto dpi_inputs = driver::ValidateDpiLinkInputs(parsed->dpi_link_sources);
  if (!dpi_inputs) {
    report(std::move(dpi_inputs.error()));
    return std::nullopt;
  }
  return Request{
      .args = *std::move(parsed), .dpi_inputs = *std::move(dpi_inputs)};
}

// What the front end made of a request: the design, where it arrived at one,
// and what it said of the source that has not been printed.
struct Elaboration {
  std::optional<frontend::ParseResult> design;
  std::string withheld;
};

// Elaborates the design the request describes, reporting whatever stops that.
// Arriving at a design is the whole of what `check` asks.
auto Elaborate(
    const Invocation& invocation, const Request& request,
    CompilerWarnings warnings) -> Elaboration {
  support::RecordWidth(request.args.compile_width);
  auto front_end = [&] {
    const profiling::StageScope stage("front end");
    const status::Phase phase("Elaborating");
    return compiler::RunFrontEnd(*invocation.driver);
  }();
  // An account that refuses the source is printed whatever the command is,
  // because then there is no program whose streams need protecting.
  const bool shows_warnings =
      warnings == CompilerWarnings::kShown || !front_end.elaborated;
  if (!shows_warnings) {
    return Elaboration{
        .design = std::move(front_end.elaborated),
        .withheld = std::move(front_end.diagnostics)};
  }
  if (!front_end.diagnostics.empty()) {
    status::Clear();
    fmt::print(stderr, "{}", front_end.diagnostics);
  }
  return Elaboration{.design = std::move(front_end.elaborated), .withheld = {}};
}

using DesignCommand = std::move_only_function<int(const CommandContext&)>;

// Carries out `command` and answers with its exit code. A failure of the
// compiler's own that no unit contained ends the command here, while the
// sources it may name are still held, and joins what the command reported.
auto Attempt(DesignCommand command, const CommandContext& ctx) -> int {
  try {
    return command(ctx);
  } catch (const std::exception& failure) {
    ctx.sink->Report(diag::InternalFailure(failure));
    return kCompilerFailureExit;
  }
}

// Elaborates the design the request describes and hands it to `command`.
// Everything the command reports goes into one sink, which is rendered here
// once the command is done. A failure of the compiler's own anywhere in the
// run, contained by a unit or not, is what the exit status says.
auto RunOnRequest(
    const Invocation& invocation, const Request& request,
    CompilerWarnings warnings, DesignCommand command) -> int {
  auto elaboration = Elaborate(invocation, request, warnings);
  if (!elaboration.design) {
    return 1;
  }
  frontend::ParseResult& elaborated = *elaboration.design;
  diag::DiagnosticSink sink;
  const int exit_code = Attempt(
      std::move(command), CommandContext{
                              .args = &request.args,
                              .elaborated = &elaborated,
                              .sink = &sink,
                              .dpi_inputs = request.dpi_inputs,
                              .program_path = invocation.program_path});
  const Reporter& report = *invocation.report;
  if (warnings == CompilerWarnings::kWithheld && !sink.HasErrors()) {
    report.WithoutWarnings()(sink);
  } else {
    // No program runs, so nothing of the compiler's is withheld any longer,
    // and what the front end said comes first because it read the source
    // first.
    fmt::print(stderr, "{}", elaboration.withheld);
    report(sink);
  }
  return sink.HasInternalErrors() ? kCompilerFailureExit : exit_code;
}

// The same for a command that asks nothing of the request before the design is
// read.
auto RunOnDesign(
    const Invocation& invocation, CompilerWarnings warnings,
    DesignCommand command) -> int {
  const auto request = ResolveRequest(invocation);
  if (!request) {
    return 1;
  }
  return RunOnRequest(invocation, *request, warnings, std::move(command));
}

// `check` asks only whether the request arrives at a design.
auto RunCheck(const Invocation& invocation) -> int {
  const auto request = ResolveRequest(invocation);
  if (!request ||
      !Elaborate(invocation, *request, CompilerWarnings::kShown).design) {
    return 1;
  }
  status::Finished();
  return 0;
}

// The program a build was asked to write is claimed before the design is read,
// so a place that cannot be written costs no compile.
auto RunBuildCommand(const Invocation& invocation) -> int {
  const auto request = ResolveRequest(invocation);
  if (!request) {
    return 1;
  }
  std::optional<driver::ClaimedFile> named;
  if (request->args.out) {
    auto claimed = driver::ClaimedFile::Claim(*request->args.out);
    if (!claimed) {
      (*invocation.report)(std::move(claimed.error()));
      return 1;
    }
    named = *std::move(claimed);
  }
  return RunOnRequest(
      invocation, *request, CompilerWarnings::kShown,
      [named = std::move(named)](const CommandContext& ctx) mutable {
        return RunBuild(ctx, std::move(named));
      });
}

// The directory a project is emitted into is made before the design is read,
// for the same reason.
auto RunEmitCommand(const Invocation& invocation) -> int {
  const auto request = ResolveRequest(invocation);
  if (!request) {
    return 1;
  }
  const std::filesystem::path& dir = *request->args.out;
  std::error_code ec;
  std::filesystem::create_directories(dir, ec);
  if (ec) {
    (*invocation.report)(diag::Make(
        diag::DiagCode::kHostIoError, "cannot write '{}' ({})", dir.string(),
        ec.message()));
    return 1;
  }
  return RunOnRequest(
      invocation, *request, CompilerWarnings::kShown, RunEmitCpp);
}

// Empties the store. It consults no design and never reaches the compiler, so
// no declaration anywhere on the machine can be read by it or stop it.
auto RunCacheClear(const Invocation& invocation) -> int {
  const Reporter& report = *invocation.report;
  auto parsed = ResolveCliOptions(
      *invocation.options, nullptr, invocation.command,
      invocation.simulation_args);
  if (!parsed) {
    report(parsed.error());
    return 1;
  }
  if (!parsed->store) {
    report(
        diag::Make(
            diag::DiagCode::kHostIoError,
            "no cache directory is known, since neither XDG_CACHE_HOME nor "
            "HOME names an absolute path")
            .WithNote("name one with '--cache-dir'"));
    return 1;
  }
  const std::size_t cleared = driver::ClearStore(*parsed->store);
  fmt::print(
      "cleared {} kept file{} from {}\n", cleared, cleared == 1 ? "" : "s",
      parsed->store->string());
  return 0;
}

}  // namespace

auto RunCommand(const Invocation& invocation) -> int {
  switch (invocation.command) {
    case CommandKind::kCheck:
      return RunCheck(invocation);
    case CommandKind::kDumpAst:
      return RunOnDesign(invocation, CompilerWarnings::kShown, RunDumpAst);
    case CommandKind::kDumpHir:
      return RunOnDesign(invocation, CompilerWarnings::kShown, RunDumpHir);
    case CommandKind::kDumpMir:
      return RunOnDesign(invocation, CompilerWarnings::kShown, RunDumpMir);
    case CommandKind::kDumpLir:
      return RunOnDesign(invocation, CompilerWarnings::kShown, RunDumpLir);
    case CommandKind::kDumpLlvm:
      return RunOnDesign(invocation, CompilerWarnings::kShown, RunDumpLlvm);
    case CommandKind::kEmitCpp:
      return RunEmitCommand(invocation);
    case CommandKind::kBuild:
      return RunBuildCommand(invocation);
    case CommandKind::kRun:
      return RunOnDesign(invocation, CompilerWarnings::kWithheld, RunProgram);
    case CommandKind::kCacheClear:
      return RunCacheClear(invocation);
  }
  throw InternalError("a command has no handler");
}

auto StartSelfReport(const CliOptions& options) -> diag::Result<SelfReport> {
  if (options.time_trace_granularity.has_value() && !options.time_trace) {
    return std::unexpected(
        diag::Make(
            diag::DiagCode::kHostInvalidCliArgs,
            "'--time-trace-granularity' has no time trace to apply to")
            .WithNote("name one with '--time-trace'"));
  }
  SelfReport report;
  if (options.time_trace) {
    // The default is clang's, so a trace of either reads at the same grain.
    constexpr std::int32_t kGranularityUs = 500;
    const std::int32_t granularity =
        options.time_trace_granularity.value_or(kGranularityUs);
    if (granularity < 0) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidCliArgs,
          "'{}' given to '--time-trace-granularity' is not a duration",
          granularity);
    }
    auto claimed = driver::ClaimedFile::Claim(*options.time_trace);
    if (!claimed) {
      return std::unexpected(std::move(claimed.error()));
    }
    report.time_trace = *std::move(claimed);
    profiling::TimeTraceStart(static_cast<unsigned>(granularity));
  }
  if (options.stats_file) {
    auto claimed = driver::ClaimedFile::Claim(*options.stats_file);
    if (!claimed) {
      return std::unexpected(std::move(claimed.error()));
    }
    report.statistics = *std::move(claimed);
    support::EnableStatistics();
  }
  return report;
}

auto WriteSelfReport(SelfReport& report) -> diag::Result<void> {
  using Write = auto (*)(const std::filesystem::path&)
                    ->std::expected<void, base::DiagMessage>;
  const auto write = [](std::optional<driver::ClaimedFile>& file,
                        Write how) -> diag::Result<void> {
    if (!file) {
      return {};
    }
    if (auto written = how(file->WorkingPath()); !written) {
      return diag::Fail(
          diag::DiagCode::kHostIoError, std::move(written.error()));
    }
    return file->Finish();
  };
  if (auto written = write(report.time_trace, profiling::TimeTraceWrite);
      !written) {
    return written;
  }
  return write(report.statistics, support::WriteStatistics);
}

}  // namespace lyra::cli
