#include "lyra/cli/commands.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <exception>
#include <expected>
#include <filesystem>
#include <format>
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
#include "lyra/driver/artifact_store.hpp"
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

// The compilers looked for when none was named, in order: clang first, because
// only it can use a precompiled header, and then the platform's own C++
// compiler, which is the name every system with one answers to.
constexpr std::array<std::string_view, 2> kUsualCompilers = {"clang++", "c++"};

// The compiler named with `--cxx`, or the first of the usual ones found on
// PATH.
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
  for (const std::string_view usual : kUsualCompilers) {
    if (auto found = driver::FindOnPath(usual)) {
      return *std::move(found);
    }
  }
  ctx.sink->Report(
      diag::Make(
          diag::DiagCode::kHostIoError,
          "no C++ compiler found: neither clang++ nor c++ is on PATH; name "
          "one with --cxx"));
  return std::nullopt;
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
      .compile_width = ctx.args->compile_width,
      .store = ctx.args->store};
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
      std::move(ctx.elaborated->compilation), ctx.elaborated->source_mapper,
      compiler::LoweringPolicy{.assertions = ctx.args->assertions}, *ctx.sink);
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
      *design, ctx.elaborated->diag_sources, *ctx.sink, ctx.args->compile_width,
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
      *design, ctx.elaborated->diag_sources, *ctx.sink, ctx.args->compile_width,
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
      *design, ctx.elaborated->diag_sources, *ctx.sink, ctx.args->compile_width,
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
      *design, ctx.elaborated->diag_sources, *ctx.sink, ctx.args->compile_width,
      text, print);
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
      ctx.dpi_inputs, host.cxx, host.optimization, host.compile_width, dir,
      dir / driver::kObjectDir / driver::kDpiSourceDir);
  if (!built) {
    ctx.sink->Report(std::move(built.error()));
    return std::nullopt;
  }
  return *std::move(built);
}

// How one backend makes the design's program: the name it is kept under,
// computed from everything the expensive half of the build reads, and that half
// itself, which runs only when nothing is kept under the name.
struct ProgramRecipe {
  driver::ContentName name;
  std::move_only_function<diag::Result<void>(const std::filesystem::path&)>
      build;
};

// The C++ backend's program. The name is read off the emitted project as the
// host compiler will read it -- every file in it, the foreign objects included
// -- and off the runtime, the compiler, and the level it compiles at.
auto CppProgramRecipe(
    const CommandContext& ctx, const driver::HostBuild& host,
    const driver::RuntimeLocation& runtime,
    const std::filesystem::path& scratch) -> std::optional<ProgramRecipe> {
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
  driver::ContentNamer namer;
  namer.AddTree("project", project);
  namer.AddTree("runtime headers", runtime.include_root);
  namer.AddFile("runtime library", runtime.lib);
  namer.AddExecutable("compiler", host.cxx);
  namer.Add("optimization", driver::OptimizationFlag(host.optimization));
  return ProgramRecipe{
      .name = namer.Finish(),
      .build = [project, units = std::move(sources->translation_units), runtime,
                foreign = *std::move(foreign),
                host](const std::filesystem::path& built) {
        const profiling::StageScope stage("host compile");
        return driver::CompileProgram(
            project, units, runtime, foreign, built, host);
      }};
}

// This compiler's own executable. The LLVM backend's code generator is part of
// it, so the objects that backend writes are a function of which build of it
// is running.
auto ThisCompiler(const CommandContext& ctx) -> std::filesystem::path {
  std::error_code ec;
  auto self = std::filesystem::read_symlink("/proc/self/exe", ec);
  return ec ? std::filesystem::path(ctx.program_path) : self;
}

// The LLVM backend's program. Each unit is taken all the way to its object as
// it is lowered -- a kept object taken in place of compiling one -- so no unit
// waits on another's, and every form a unit passed through is released as it
// goes.
//
// The program is named by the names of the objects it links, the runtime they
// link against, the driver that links them, and the foreign objects beside
// them, so what is kept at this level saves the link. How many units are built
// at once changes none of the objects, so it is in no name.
auto LlvmProgramRecipe(
    const CommandContext& ctx, const driver::HostBuild& host,
    const driver::RuntimeLocation& runtime,
    const std::filesystem::path& scratch) -> std::optional<ProgramRecipe> {
  auto design = DesignOf(ctx);
  if (!design) {
    return std::nullopt;
  }
  driver::ContentNamer generator;
  generator.AddExecutable("code generator", ThisCompiler(ctx));
  const program::ObjectBuild objects{
      .optimization = host.optimization,
      .code_generator = generator.Finish(),
      .store = host.store,
      .reuse_kept = !ctx.args->rebuild,
      .object_dir = scratch / driver::kObjectDir};
  std::error_code created;
  std::filesystem::create_directories(objects.object_dir, created);
  if (created) {
    ctx.sink->Report(
        diag::Make(
            diag::DiagCode::kHostIoError,
            std::format(
                "failed to create '{}': {}", objects.object_dir.string(),
                created.message())));
    return std::nullopt;
  }
  program::ProgramSink sink;
  const status::Phase phase("Compiling");
  auto lowered = compiler::LowerToSemantic(
      *design, ctx.elaborated->diag_sources, *ctx.sink, host.compile_width,
      [&objects](mir::CompilationUnit unit) {
        return program::BuildUnit(unit, objects);
      },
      [&sink](program::BuiltUnit unit) { sink.Collect(std::move(unit)); });
  if (!lowered) {
    return std::nullopt;
  }
  auto built = [&] {
    const profiling::StageScope stage("generate root object");
    return std::move(sink).Finish(lowered->root, objects);
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
  driver::ContentNamer namer;
  std::vector<std::filesystem::path> linked;
  linked.reserve(built->objects.size() + foreign->size());
  for (const program::ObjectFile& object : built->objects) {
    namer.Add("object", object.name.hex);
    linked.push_back(object.path);
  }
  namer.AddFile("runtime library", runtime.lib);
  namer.AddExecutable("linker", host.cxx);
  for (const std::filesystem::path& object : *foreign) {
    namer.AddFile("foreign object", object);
    linked.push_back(object);
  }
  return ProgramRecipe{
      .name = namer.Finish(),
      .build = [linked = std::move(linked), runtime_lib = runtime.lib,
                cxx = host.cxx](const std::filesystem::path& program) {
        const profiling::StageScope stage("link");
        return driver::LinkProgram(linked, runtime_lib, program, cxx);
      }};
}

auto RecipeFor(
    const CommandContext& ctx, const driver::HostBuild& host,
    const driver::RuntimeLocation& runtime,
    const std::filesystem::path& scratch) -> std::optional<ProgramRecipe> {
  switch (ctx.args->backend) {
    case Backend::kCpp:
      return CppProgramRecipe(ctx, host, runtime, scratch);
    case Backend::kLlvm:
      return LlvmProgramRecipe(ctx, host, runtime, scratch);
  }
  throw InternalError("the request names no backend");
}

// Places the design's program at `destination`: a copy of the one kept under
// its name when there is one and a rebuild was not asked for, and otherwise
// one built now, kept, and copied from what was built.
auto PlaceProgram(
    const CommandContext& ctx, const std::filesystem::path& scratch,
    const std::filesystem::path& destination) -> bool {
  auto host = ResolveHostBuild(ctx);
  if (!host) {
    return false;
  }
  auto runtime = ResolveRuntime(ctx);
  if (!runtime) {
    return false;
  }
  auto recipe = RecipeFor(ctx, *host, *runtime, scratch);
  if (!recipe) {
    return false;
  }
  const auto& store = host->store;
  if (store && !ctx.args->rebuild) {
    auto kept = driver::CopyStored(
        *store, driver::kStoredProgramDir, recipe->name, destination);
    if (!kept) {
      ctx.sink->Report(std::move(kept.error()));
      return false;
    }
    if (*kept) {
      return true;
    }
  }
  const std::filesystem::path built = scratch / driver::kProgramName;
  if (auto made = recipe->build(built); !made) {
    ctx.sink->Report(std::move(made.error()));
    return false;
  }
  if (store) {
    driver::KeepStored(*store, driver::kStoredProgramDir, recipe->name, built);
  }
  auto copied = driver::CopyOut(built, destination);
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
          std::format(
              "this design has {} tops and no name, so the program has none "
              "to take; name it with -o",
              tops.size())));
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
  auto scratch = ScratchDir(ctx);
  if (!scratch || !PlaceProgram(ctx, scratch->Path(), named->WorkingPath())) {
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

// Runs a copy of the design's program that belongs to this run alone, so
// nothing another process does to what is kept can reach it.
auto RunProgram(const CommandContext& ctx) -> int {
  auto scratch = ScratchDir(ctx);
  if (!scratch) {
    return 1;
  }
  const std::filesystem::path program = scratch->Path() / "run";
  if (!PlaceProgram(ctx, scratch->Path(), program)) {
    return 1;
  }
  status::Finished();
  auto exit_code =
      driver::RunProcessStreaming(program, ctx.args->simulation_args);
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
// outright. A remark was asked for, so it is not withheld.
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
    report(diag::Make(diag::DiagCode::kHostInvalidCliArgs, parsed.error()));
    return std::nullopt;
  }

  if (!driver.sourceLoader.hasFiles()) {
    auto diagnostic =
        diag::Make(diag::DiagCode::kHostNoInputFiles, "no input files");
    if (absent) {
      diagnostic =
          std::move(diagnostic)
              .WithNote(
                  std::format(
                      "searched for lyra.toml from {} up to {}",
                      absent->started.string(), absent->stopped.string()));
    }
    // A declaration was in effect and still named nothing, which reads as no
    // declaration at all unless the message says which one applied -- and the
    // one that applied may be several directories above the caller.
    if (declared != nullptr) {
      diagnostic = std::move(diagnostic)
                       .WithNote(
                           std::format(
                               "library '{}' at {} declares no source files",
                               declared->root.library.name,
                               declared->root.path.string()));
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

// Elaborates the design the request describes, reporting whatever stops that.
// Arriving at a design is the whole of what `check` asks.
auto Elaborate(
    const Invocation& invocation, const Request& request,
    CompilerWarnings warnings) -> std::optional<frontend::ParseResult> {
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
  if (shows_warnings && !front_end.diagnostics.empty()) {
    status::Clear();
    fmt::print(stderr, "{}", front_end.diagnostics);
  }
  return std::move(front_end.elaborated);
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
  auto elaborated = Elaborate(invocation, request, warnings);
  if (!elaborated) {
    return 1;
  }
  diag::DiagnosticSink sink;
  const int exit_code = Attempt(
      std::move(command), CommandContext{
                              .args = &request.args,
                              .elaborated = &*elaborated,
                              .sink = &sink,
                              .dpi_inputs = request.dpi_inputs,
                              .program_path = invocation.program_path});
  const Reporter& report = *invocation.report;
  if (warnings == CompilerWarnings::kWithheld && !sink.HasErrors()) {
    report.WithoutWarnings()(sink, &elaborated->diag_sources);
  } else {
    report(sink, &elaborated->diag_sources);
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
  if (!request || !Elaborate(invocation, *request, CompilerWarnings::kShown)) {
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
        diag::DiagCode::kHostIoError,
        std::format("cannot write '{}': {}", dir.string(), ec.message())));
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
    report(diag::Make(diag::DiagCode::kHostInvalidCliArgs, parsed.error()));
    return 1;
  }
  if (!parsed->store) {
    report(
        diag::Make(
            diag::DiagCode::kHostIoError,
            "no cache directory is known: neither XDG_CACHE_HOME nor HOME "
            "names an absolute path; name one with --cache-dir"));
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
    return diag::Fail(
        diag::DiagCode::kHostInvalidCliArgs,
        "--time-trace-granularity: there is no time trace to apply it to; "
        "name one with --time-trace");
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
          std::format(
              "--time-trace-granularity: '{}' is not a duration", granularity));
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
  using Write =
      auto (*)(const std::filesystem::path&)->std::expected<void, std::string>;
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
