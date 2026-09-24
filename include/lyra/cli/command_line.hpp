#pragma once

#include <cstddef>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <optional>
#include <span>
#include <string>
#include <variant>
#include <vector>

#include <slang/driver/Driver.h>
#include <slang/util/CommandLine.h>

#include "lyra/cli/design_manifest.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace lyra::cli {

enum class CommandKind : std::uint8_t {
  kCheck,
  kDumpAst,
  kDumpHir,
  kDumpMir,
  kDumpLir,
  kDumpLlvm,
  kEmitCpp,
  kBuild,
  kRun,
  kCacheClear,
};

// Which backend turns the design into a program. The C++ backend emits a C++
// project and builds it with the host compiler; the LLVM backend compiles each
// unit to a module itself and links them with the host's driver. Either way
// the product is one program, which `build` hands back and `run` executes.
enum class Backend : std::uint8_t { kCpp, kLlvm };

struct ParsedArgs {
  Backend backend = Backend::kCpp;
  support::AssertionPolicy assertions = support::AssertionPolicy::kCheck;
  driver::SourceFormatting formatting = driver::SourceFormatting::kOff;
  driver::Optimization optimization = driver::Optimization::kIterate;
  driver::pch::Policy pch = driver::pch::Policy::kAttempt;
  // The host C++ compiler a program is compiled or linked with, when the caller
  // named one: a program name or path, never flags.
  std::optional<std::string> cxx;
  // How many of the design's translation units the host compiler may work on
  // at once. Already resolved to a positive count here, so nothing downstream
  // reads a zero as a request for the processor count.
  std::size_t compile_width = 1;
  // Where the command writes what it produces, when the caller named a place:
  // the program for `build`, the project directory for `emit cpp`.
  std::optional<std::filesystem::path> out;
  // Where programs and prepared headers are kept for reuse. Absent when no
  // cache directory is known, and then nothing is kept.
  std::optional<std::filesystem::path> store;
  // Build as though the store held nothing, and keep what was built.
  bool rebuild = false;
  // The name the design declares itself under, for a design that has one.
  std::optional<std::string> design_name;
  // The simulation's own arguments, which is where LRM 21.6 plusargs land: the
  // program takes them as its argv. Everything after a standalone `--`, so a
  // simulation argument never has to be told apart from a compiler one by its
  // spelling.
  std::vector<std::string> simulation_args;
  // LRM 35 DPI-C link inputs: native source files (`.c` / `.cpp`) providing the
  // foreign symbols an `import "DPI-C"` calls. Compiled and linked into the
  // program alongside the design's own objects.
  std::vector<std::string> dpi_link_sources;
};

// What Lyra adds to the front end's command line. Every field is optional
// because that is how the parser says "not given", which is a different fact
// from the default Lyra then chooses.
struct CliOptions {
  std::optional<bool> color;
  std::optional<bool> no_color;
  std::optional<bool> format;
  std::optional<std::string> assertions;
  std::optional<bool> no_pch;
  std::optional<bool> release;
  std::optional<std::string> config;
  std::optional<std::string> cache_dir;
  std::optional<bool> rebuild;
  std::optional<std::string> cxx;
  std::optional<int32_t> jobs;
  std::optional<std::string> out;
  std::optional<std::string> backend;
  std::vector<std::string> dpi_link;
};

// What Lyra parses and what it hands the simulation, told apart by a standalone
// `--`. Splitting there keeps the front end from ever having to decide whether
// a `+`-prefixed word is one of its options or one of the design's plusargs.
struct SplitArgv {
  std::vector<char*> lyra;
  std::vector<std::string> child;
};

auto SplitAtSeparator(std::span<char* const> raw) -> SplitArgv;

auto WantsHelp(std::span<char* const> words) -> bool;

// The one merged option list: Lyra's own options and every front-end option
// inherited alongside them. Asking the parser to render it is what keeps the
// help honest about which options a build may actually pass.
void PrintHelp(slang::driver::Driver& driver);

// Registers Lyra's own options beside the front end's on one command line, so
// a build that already knows how to describe a design to slang describes it to
// Lyra the same way, and one help text covers both.
void RegisterCliOptions(slang::CommandLine& cmd, CliOptions& opts);

// One pass over Lyra's side of the command line: the command words first --
// they are positional -- then the options, through the parser both halves
// share. `words` is consumed: the command words are removed from it.
auto ParseCommandWords(slang::driver::Driver& driver, std::vector<char*>& words)
    -> std::expected<CommandKind, std::string>;

// Refuses, by name, every one of Lyra's options the caller gave that the
// command acts on none of, naming the commands that do. An option a command
// acts on is never refused for having no effect this time -- `--no-pch` given
// to a build on the backend that compiles no C++ -- because what is refused has
// to be a function of the command alone for a caller to predict it.
// `has_simulation_args` says whether anything followed a standalone `--`.
auto RefuseOptionsNotTaken(
    const CliOptions& opts, CommandKind cmd, bool has_simulation_args)
    -> std::expected<void, std::string>;

// Answered from what the parser recorded, so a command line that fails to
// resolve can still report why in the colour the caller asked for.
auto UseColor(const CliOptions& opts) -> bool;

// Nothing was searched for, because the command line named its own sources.
// This is not the absence of a search: there is nowhere to report having
// looked, and no declaration elsewhere on the machine could have applied.
struct NoSearchNeeded {};

using DesignDeclaration =
    std::variant<DesignManifest, ManifestAbsent, NoSearchNeeded>;

// A declaration named outright is read whatever else was said. Otherwise one is
// searched for, and only when the command line named no source of its own --
// which is what makes an invocation independent of every file outside the
// inputs it names.
auto ResolveDesignDeclaration(
    const CliOptions& opts, const slang::driver::Driver& driver)
    -> diag::Result<DesignDeclaration>;

// Adds a declaration's material to what the command line already named, and
// makes a selection only where the command line left one unmade.
//
// Call this after the command line is parsed. The order is load-bearing: the
// front end's parser keeps the first value a single-valued option is given, so
// applying a declaration ahead of the command line makes it win every selection
// rather than lose them.
auto ApplyDesignManifest(
    const DesignManifest& manifest, slang::driver::Driver& driver)
    -> diag::Result<void>;

// Turns what the parser recorded into the choices the rest of the run needs,
// applying Lyra's defaults and rejecting a value no command can act on. A
// design's declaration is read first and the command line over it, which is the
// whole of the precedence rule: material accumulates, selection replaces.
auto ResolveCliOptions(
    const CliOptions& opts, const DesignManifest* manifest, CommandKind cmd,
    std::span<const std::string> simulation_args)
    -> std::expected<ParsedArgs, std::string>;

}  // namespace lyra::cli
