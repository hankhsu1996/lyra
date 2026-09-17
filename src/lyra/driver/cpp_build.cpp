#include "lyra/driver/cpp_build.hpp"

#include <array>
#include <cstddef>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/driver/file_output.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/support/runtime_prelude.hpp"
#include "lyra/support/subprocess.hpp"

namespace lyra::driver {

namespace {

auto IoError(std::string message) {
  return diag::Fail(diag::DiagCode::kHostIoError, std::move(message));
}

// Replace every `@KEY@` occurrence in `tpl` with its mapped substitution.
// Tokens left unbound are passed through unchanged (build.sh shell syntax
// uses `${VAR}` and `$VAR`, never the `@...@` form, so unbound tokens here
// are author bugs rather than legal shell). Kept inline because the script
// is the only caller; promoting to a public utility waits for a second use.
auto SubstituteTokens(
    std::string_view tpl,
    std::span<const std::pair<std::string_view, std::string_view>> bindings)
    -> std::string {
  std::string out;
  out.reserve(tpl.size());
  std::size_t i = 0;
  while (i < tpl.size()) {
    if (tpl[i] != '@') {
      out.push_back(tpl[i]);
      ++i;
      continue;
    }
    const auto end = tpl.find('@', i + 1);
    if (end == std::string_view::npos) {
      out.append(tpl.substr(i));
      break;
    }
    // The key carries its own delimiters, so a binding is written and matched
    // in the one spelling the template uses.
    const auto key = tpl.substr(i, end - i + 1);
    bool replaced = false;
    for (const auto& [k, v] : bindings) {
      if (key == k) {
        out.append(v);
        replaced = true;
        break;
      }
    }
    if (!replaced) out.append(key);
    i = end + 1;
  }
  return out;
}

// build.sh as a raw-string template. `@TOKEN@` placeholders bind to the
// `project_layout` constants below. Shell `${VAR}` and `$VAR` pass through
// unchanged. The PCH section gates on three runtime checks: the caller not
// having asked to skip it, `$CXX` looking like clang (gcc's PCH dialect does
// not match these flags), and `sha1sum` being available (used to fingerprint
// the header tree so a header edit produces a different cache file rather
// than reusing a PCH built against stale-on-disk content). Any failing
// check falls back to plain compilation; correctness is unaffected.
//
// The compile loop waits for a batch rather than replacing each finished job,
// because `wait -n` is not POSIX and this recipe may not assume a shell richer
// than /bin/sh. At one job the batch is one and the loop is sequential, which
// is why there is no second path for that case.
constexpr std::string_view kBuildScriptTemplate = R"sh(#!/bin/sh
# Build this self-contained Lyra C++ project.
#
#   usage: build.sh [--cxx <compiler>] [--no-pch] [-j <jobs>]
#
# The compiler that produced this project is baked in below and is the default.
# Nothing here reads the environment: what this script does is determined by the
# file plus its arguments, so a rebuild is reproducible and a stray $CXX in some
# shell cannot silently change which toolchain builds the design.
#
# Moving the project to a machine where that path means nothing? Pass --cxx. If
# the compiler is not a conforming C++23 implementation the build fails inside
# the runtime headers -- point --cxx at a wrapper script adding whatever it
# needs (--gcc-install-dir=, -stdlib=libc++, --sysroot=).
#
# Each unit is compiled on its own and the objects are linked. -j says how many
# of those compiles may run at once; -j 0 asks for one per processor. One at a
# time is the default, because a script that was told nothing cannot know what
# else is running on the machine.
#
# A precompiled header is built on first run and reused on later rebuilds to
# amortize parsing of the runtime headers (clang only); --no-pch skips it.
set -e
CXX="@CXX@"
NO_PCH=0
JOBS=1
while [ $# -gt 0 ]; do
  case "$1" in
    --cxx)
      if [ $# -lt 2 ]; then echo "build.sh: --cxx needs a value" >&2; exit 2; fi
      CXX="$2"; shift 2 ;;
    --no-pch) NO_PCH=1; shift ;;
    -j)
      if [ $# -lt 2 ]; then echo "build.sh: -j needs a value" >&2; exit 2; fi
      JOBS="$2"; shift 2 ;;
    *)
      echo "usage: build.sh [--cxx <compiler>] [--no-pch] [-j <jobs>]" >&2
      exit 2 ;;
  esac
done
case "$JOBS" in
  ''|*[!0-9]*) echo "build.sh: -j needs a count" >&2; exit 2 ;;
esac
if [ "$JOBS" = "0" ]; then
  JOBS=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)
fi
USE_PCH=0
if [ "$NO_PCH" = "0" ]; then
  case "$CXX" in
    *clang*)
      if command -v sha1sum >/dev/null 2>&1; then USE_PCH=1; fi
      ;;
  esac
fi
PCH_FLAG=""
if [ "$USE_PCH" = "1" ]; then
  PRELUDE="@INCLUDE@/@PRELUDE@"
  FP=$(find @INCLUDE@ -name '*.hpp' -print0 | sort -z | xargs -0 sha1sum | sha1sum | cut -c1-16)
  PCH="@CACHE@/prelude-${FP}-@OPTTAG@.pch"
  if [ ! -f "$PCH" ]; then
    mkdir -p "$(dirname "$PCH")"
    "$CXX" @STD@ @OPT@ @VALIDATE@ -I @INCLUDE@ -xc++-header "$PRELUDE" -o "$PCH"
  fi
  PCH_FLAG="-include-pch $PCH @VALIDATE@"
fi
mkdir -p @OBJDIR@
LOG=@OBJDIR@/compile.log
reap() {
  for p in $PIDS; do
    wait "$p" || STATUS=1
  done
  PIDS=""
  LIVE=0
}
compile_all() {
  : > "$LOG"
  OBJS=""
  PIDS=""
  LIVE=0
  STATUS=0
  for src in @SOURCES@; do
    OBJS="$OBJS @OBJDIR@/$src.o"
    "$CXX" @STD@ @OPT@ -I @INCLUDE@ $1 -c "$src" -o "@OBJDIR@/$src.o" \
      >>"$LOG" 2>&1 &
    PIDS="$PIDS $!"
    LIVE=$((LIVE + 1))
    if [ "$LIVE" -ge "$JOBS" ]; then reap; fi
  done
  reap
}
compile_all "$PCH_FLAG"
if [ "$STATUS" -ne 0 ] && [ "$USE_PCH" = "1" ]; then
  # The precompiled header is an attempt and never a requirement, so it does not
  # get to decide whether this build succeeds. Compile again the way that needs
  # nothing prepared; if that works, the header was refused rather than the
  # sources, and it goes so the next build prepares it afresh. Everything is
  # compiled again rather than only what failed, because tracking that costs
  # more shell than the rare path is worth.
  compile_all ""
  if [ "$STATUS" -eq 0 ]; then rm -f "$PCH"; fi
fi
# What the compiler said is kept back until the build is known to have failed,
# so that an attempt which was retried and succeeded says nothing at all. What
# gets shown is the last attempt, which is the one that needed nothing prepared
# and so describes the sources rather than the header.
if [ "$STATUS" -ne 0 ]; then cat "$LOG" >&2; exit 1; fi
@DPICOMPILE@"$CXX"$OBJS@DPIOBJS@ @LIBDIR@/@LIB@ -o @PROG@
)sh";

// Words as one shell list, space separated. A list with nothing in it renders
// as nothing, which is what lets the recipe hold no branch for a design that
// contributes none.
auto JoinWords(std::span<const std::string> words) -> std::string {
  std::string out;
  for (const std::string& word : words) {
    if (!out.empty()) out += " ";
    out += word;
  }
  return out;
}

// Where a DPI-C source sits once copied into the project, relative to it. The
// recipe and the copy read the location from here, so neither restates it.
auto DpiSourceRelPath(const DpiLinkInput& input) -> std::string {
  return std::format("{}/{}", kDpiSourceDir, input.source.filename().string());
}

// Which language a foreign source is compiled as, and the standard where its
// language has one to name (LRM 35). A C source is compiled as C so its symbols
// keep C linkage, which is what the emitted declaration expects and what a C++
// compilation would mangle away.
auto ForeignLanguageFlags(const DpiLinkInput& input)
    -> std::vector<std::string> {
  if (input.compile_as_c) {
    return {"-x", "c"};
  }
  return {std::string(kCxxStandardFlag), "-x", "c++"};
}

// Where a foreign source's object lands, relative to the project.
auto DpiObjectRelPath(const DpiLinkInput& input) -> std::string {
  return std::format(
      "{}/{}/{}.o", kObjectDir, kDpiSourceDir,
      input.source.filename().string());
}

// The build recipe's DPI-C contribution (LRM 35), rendered from the already
// classified link inputs so the script carries no language detection of its
// own. Both halves are empty for a design with no foreign sources, which is why
// the recipe needs no branch for that case. A foreign source compiles against
// the one include path the project publishes its boundary on, which from the
// recipe's own directory is `.`.
struct DpiRecipe {
  std::string compile_steps;
  std::string link_inputs;
};

auto RenderDpiRecipe(
    std::span<const DpiLinkInput> inputs, std::string_view optimization_flag)
    -> DpiRecipe {
  DpiRecipe recipe;
  for (const DpiLinkInput& input : inputs) {
    const std::string object = DpiObjectRelPath(input);
    recipe.compile_steps += std::format(
        "mkdir -p {}/{}\n\"$CXX\" {} {} -c {} -I . -o {}\n", kObjectDir,
        kDpiSourceDir, JoinWords(ForeignLanguageFlags(input)),
        optimization_flag, DpiSourceRelPath(input), object);
    recipe.link_inputs += std::format(" {}", object);
  }
  return recipe;
}

auto RenderBuildScript(
    const std::filesystem::path& cxx, std::span<const DpiLinkInput> dpi_inputs,
    std::span<const std::string> translation_units, Optimization optimization)
    -> std::string {
  const std::string_view optimization_flag = OptimizationFlag(optimization);
  const DpiRecipe dpi = RenderDpiRecipe(dpi_inputs, optimization_flag);
  const std::string sources = JoinWords(translation_units);
  // Named locals, because the bindings below hold `string_view`s and are read
  // after this statement: a temporary would already have died.
  const std::string cxx_exe = cxx.string();
  // The recipe keys its own PCH cache by header content, which does not
  // separate two builds clang will refuse to share.
  const std::string_view optimization_tag = optimization_flag.substr(1);
  const std::array<std::pair<std::string_view, std::string_view>, 15> bindings =
      {{
          {"@INCLUDE@", kRuntimeIncludeDir},
          {"@PRELUDE@", support::kRuntimePreludeHeader},
          {"@CACHE@", kRuntimeCacheDir},
          {"@OBJDIR@", kObjectDir},
          {"@STD@", kCxxStandardFlag},
          {"@OPT@", optimization_flag},
          {"@VALIDATE@", kPchContentValidationFlag},
          {"@OPTTAG@", optimization_tag},
          {"@CXX@", cxx_exe},
          {"@SOURCES@", sources},
          {"@LIBDIR@", kRuntimeLibDir},
          {"@LIB@", kRuntimeLibFile},
          {"@PROG@", kProgramName},
          {"@DPICOMPILE@", dpi.compile_steps},
          {"@DPIOBJS@", dpi.link_inputs},
      }};
  return SubstituteTokens(kBuildScriptTemplate, bindings);
}

// Reformats the emitted C++ files in place with clang-format. Reached only
// when the caller asked for formatting, which makes clang-format a tool this
// command needs rather than a nicety, so a missing one is reported the same way
// a missing host compiler is. A non-zero exit says clang-format could not read
// or write files Lyra has just written, which is about the emission and not
// about style, so it is not swallowed either.
auto FormatSources(
    std::span<const std::string> relpaths, const std::filesystem::path& dir)
    -> diag::Result<void> {
  auto clang_format = support::FindOnPath("clang-format");
  if (!clang_format) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, std::move(clang_format.error()));
  }
  std::vector<std::string> args = {"-i", "-style=Google"};
  for (const std::string& relpath : relpaths) {
    args.push_back((dir / relpath).string());
  }
  auto run = support::RunProcessCaptured(*clang_format, args);
  if (!run) {
    return diag::Fail(diag::DiagCode::kHostIoError, std::move(run.error()));
  }
  if (run->exit_code != 0) {
    return diag::Fail(
        diag::DiagCode::kHostIoError,
        std::format(
            "clang-format failed on the emitted sources: {}",
            run->stderr_text));
  }
  return {};
}

// Copies the user's DPI-C sources into the project (LRM 35) so the directory
// builds on another machine: the recipe reaches them by a project-relative
// path, which an absolute path back to the originals could not survive.
auto CopyDpiSources(
    std::span<const DpiLinkInput> inputs, const std::filesystem::path& dir)
    -> diag::Result<void> {
  for (const DpiLinkInput& input : inputs) {
    if (auto r = CopyFile(input.source, dir / DpiSourceRelPath(input)); !r) {
      return r;
    }
  }
  return {};
}

// One compile the build has to run, what the link takes from it, and what to
// name if it fails. A unit's compile and a foreign source's are both this, so
// one bounded run covers every compile a build does.
//
// `plain` needs nothing prepared in advance and therefore always works. `fast`
// is the same compile handed a precompiled header, which the compiler may
// refuse for reasons about the header rather than about the source. Not every
// compile has one -- a foreign source includes none of what such a header holds
// -- so the two are separate rather than one command line with a flag.
struct CompileStep {
  support::ProcessRequest plain;
  std::optional<support::ProcessRequest> fast;
  std::string subject;
  std::string object;
};

// What compiling one translation unit costs the host compiler, as a request
// rather than a run, so the caller decides how many happen at once.
auto UnitCompileStep(
    const std::filesystem::path& dir, const std::string& source,
    const std::filesystem::path& include_root, const HostBuild& host,
    const std::optional<std::filesystem::path>& prelude) -> CompileStep {
  const std::string object = (dir / kObjectDir / (source + ".o")).string();
  const auto command =
      [&](const std::optional<std::filesystem::path>& prepared) {
        std::vector<std::string> args = {
            std::string(kCxxStandardFlag),
            std::string(OptimizationFlag(host.optimization)), "-I",
            include_root.string()};
        if (prepared.has_value()) {
          args.emplace_back("-include-pch");
          args.push_back(prepared->string());
          args.emplace_back(kPchContentValidationFlag);
        }
        args.emplace_back("-c");
        args.push_back((dir / source).string());
        args.emplace_back("-o");
        args.push_back(object);
        return support::ProcessRequest{
            .exe = host.cxx, .args = std::move(args)};
      };
  return CompileStep{
      .plain = command(std::nullopt),
      .fast = prelude.has_value()
                  ? std::optional<support::ProcessRequest>{command(prelude)}
                  : std::nullopt,
      .subject = source,
      .object = object};
}

// What compiling one DPI-C link input costs (LRM 35). The project's own
// directory is the one include path it publishes its foreign boundary on, which
// is the whole of what such a source compiles against.
auto DpiCompileStep(
    const std::filesystem::path& dir, const DpiLinkInput& input,
    const HostBuild& host) -> CompileStep {
  const std::string source = input.source.string();
  const std::string object = (dir / DpiObjectRelPath(input)).string();
  std::vector<std::string> args = ForeignLanguageFlags(input);
  args.emplace_back(OptimizationFlag(host.optimization));
  args.emplace_back("-c");
  args.push_back(source);
  args.emplace_back("-I");
  args.push_back(dir.string());
  args.emplace_back("-o");
  args.push_back(object);
  return CompileStep{
      .plain = {.exe = host.cxx, .args = std::move(args)},
      .fast = std::nullopt,
      .subject = source,
      .object = object};
}

// Runs every compile, as many at once as this host was told to take, and
// reports every one that failed rather than the first. A build whose emitted
// text does not compile is a defect in Lyra, and seeing all of them at once is
// what saves the run it would otherwise take to find the next.
//
// A header compiled in advance may not decide whether a build succeeds, so a
// compile that failed with one is run again without it before its output counts
// as a failure. A compile that then succeeds was refused the header rather than
// the source, and the header is dropped so the next build makes a fresh one.
// Where no header was offered there is nothing to run again, and the second
// pass is empty without being asked to be.
auto RunCompileSteps(
    std::span<const CompileStep> steps, const HostBuild& host,
    const std::optional<std::filesystem::path>& prelude) -> diag::Result<void> {
  std::vector<support::ProcessRequest> requests;
  requests.reserve(steps.size());
  for (const CompileStep& step : steps) {
    std::error_code ec;
    const std::filesystem::path home =
        std::filesystem::path(step.object).parent_path();
    std::filesystem::create_directories(home, ec);
    if (ec) {
      return diag::Fail(
          diag::DiagCode::kHostIoError,
          std::format(
              "failed to create '{}': {}", home.string(), ec.message()));
    }
    requests.push_back(step.fast.value_or(step.plain));
  }
  auto results = support::RunProcessesCaptured(requests, host.compile_width);
  if (!results) {
    return IoError(std::move(results.error()));
  }

  std::vector<std::size_t> retried;
  for (std::size_t i = 0; i < steps.size(); ++i) {
    if ((*results)[i].exit_code != 0 && steps[i].fast.has_value()) {
      retried.push_back(i);
    }
  }
  if (!retried.empty()) {
    std::vector<support::ProcessRequest> plain;
    plain.reserve(retried.size());
    for (std::size_t i : retried) {
      plain.push_back(steps[i].plain);
    }
    auto plain_results =
        support::RunProcessesCaptured(plain, host.compile_width);
    if (!plain_results) {
      return IoError(std::move(plain_results.error()));
    }
    bool the_header_was_refused = false;
    for (std::size_t k = 0; k < retried.size(); ++k) {
      the_header_was_refused |= (*plain_results)[k].exit_code == 0;
      (*results)[retried[k]] = std::move((*plain_results)[k]);
    }
    if (the_header_was_refused && prelude.has_value()) {
      pch::Discard(*prelude);
    }
  }

  std::string failures;
  for (std::size_t i = 0; i < steps.size(); ++i) {
    if ((*results)[i].exit_code == 0) {
      continue;
    }
    if (!failures.empty()) {
      failures += "\n";
    }
    failures += std::format(
        "compiling '{}' failed:\n{}", steps[i].subject,
        (*results)[i].stderr_text);
  }
  if (!failures.empty()) {
    return diag::Fail(diag::DiagCode::kHostBuildFailed, std::move(failures));
  }
  return {};
}

auto CompileProgram(
    const std::filesystem::path& dir,
    std::span<const std::string> translation_units,
    const std::filesystem::path& include_root, const std::filesystem::path& lib,
    const std::filesystem::path& program, const HostBuild& host,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<void> {
  // The prelude is compiled before anything reads it, so several compiles
  // cannot each find it missing and race to build the same file.
  const std::optional<std::filesystem::path> prelude =
      pch::EnsureCached(host.cxx, include_root, host.pch, host.optimization);

  std::vector<CompileStep> steps;
  for (const std::string& source : translation_units) {
    steps.push_back(UnitCompileStep(dir, source, include_root, host, prelude));
  }
  for (const DpiLinkInput& input : dpi_inputs) {
    steps.push_back(DpiCompileStep(dir, input, host));
  }

  if (auto r = RunCompileSteps(steps, host, prelude); !r) {
    return r;
  }

  std::vector<std::string> args;
  args.reserve(steps.size() + 3);
  for (const CompileStep& step : steps) {
    args.push_back(step.object);
  }
  args.push_back(lib.string());
  args.emplace_back("-o");
  args.push_back(program.string());
  auto result_or = support::RunProcessCaptured(host.cxx, args);
  if (!result_or) {
    return IoError(std::move(result_or.error()));
  }
  if (result_or->exit_code != 0) {
    return diag::Fail(
        diag::DiagCode::kHostBuildFailed,
        std::format("linking the program failed:\n{}", result_or->stderr_text));
  }
  return {};
}

}  // namespace

auto CppProjectSink::Take(const mir::CompilationUnit& unit)
    -> diag::Result<void> {
  if (auto refusal = backend::cpp::RefusalFor(unit); refusal.has_value()) {
    return std::unexpected(std::move(*refusal));
  }
  if (auto r = WriteUnit(unit); !r) {
    return r;
  }
  dpi::CollectAbiFragment(unit, dpi_fragments_);
  return {};
}

auto CppProjectSink::Finish(const mir::CompilationUnit& root)
    -> diag::Result<void> {
  if (auto r = WriteUnit(root); !r) {
    return r;
  }
  if (auto r = WriteTranslationUnit(backend::cpp::EmitCppHostMain(root)); !r) {
    return r;
  }
  if (formatting_ == SourceFormatting::kOn) {
    return FormatSources(written_, dir_);
  }
  return {};
}

auto CppProjectSink::WriteUnit(const mir::CompilationUnit& unit)
    -> diag::Result<void> {
  backend::cpp::CppUnitArtifacts files = backend::cpp::EmitCppUnit(unit);
  if (auto r = Write(std::move(files.signature)); !r) {
    return r;
  }
  return WriteTranslationUnit(std::move(files.code));
}

auto CppProjectSink::WriteTranslationUnit(backend::cpp::CppArtifact file)
    -> diag::Result<void> {
  translation_units_.push_back(file.relpath);
  return Write(std::move(file));
}

auto CppProjectSink::Write(backend::cpp::CppArtifact file)
    -> diag::Result<void> {
  if (auto r = WriteFile(dir_ / file.relpath, file.content); !r) {
    return r;
  }
  written_.push_back(std::move(file.relpath));
  return {};
}

auto AssembleProject(
    const RuntimeLocation& runtime, const EmittedCppSources& sources,
    const std::filesystem::path& dir, const HostBuild& host,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<void> {
  if (auto r = WriteDpiSurface(runtime, sources.dpi_fragments, dir); !r) {
    return r;
  }
  if (auto r = CopyDpiSources(dpi_inputs, dir); !r) {
    return r;
  }

  const auto script_path = dir / "build.sh";
  if (auto r = WriteFile(
          script_path, RenderBuildScript(
                           host.cxx, dpi_inputs, sources.translation_units,
                           host.optimization));
      !r) {
    return r;
  }
  std::error_code ec;
  std::filesystem::permissions(
      script_path,
      std::filesystem::perms::owner_exec | std::filesystem::perms::group_exec |
          std::filesystem::perms::others_exec,
      std::filesystem::perm_options::add, ec);
  if (ec) {
    return IoError(
        std::format(
            "failed to mark '{}' executable: {}", script_path.string(),
            ec.message()));
  }

  return ExportRuntimeTree(runtime, dir);
}

auto BuildProject(
    const std::filesystem::path& dir,
    std::span<const std::string> translation_units, const HostBuild& host,
    std::span<const DpiLinkInput> dpi_inputs)
    -> diag::Result<std::filesystem::path> {
  const auto program = dir / kProgramName;
  if (auto r = CompileProgram(
          dir, translation_units, dir / kRuntimeIncludeDir,
          dir / kRuntimeLibDir / kRuntimeLibFile, program, host, dpi_inputs);
      !r) {
    return std::unexpected(std::move(r.error()));
  }
  return program;
}

auto RunInPlace(
    const RuntimeLocation& runtime, const EmittedCppSources& sources,
    const std::filesystem::path& work_dir, const HostBuild& host,
    std::span<const std::string> child_args,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<int> {
  if (auto r = WriteDpiSurface(runtime, sources.dpi_fragments, work_dir); !r) {
    return std::unexpected(std::move(r.error()));
  }
  const auto program = work_dir / kProgramName;
  if (auto r = CompileProgram(
          work_dir, sources.translation_units, runtime.include_root,
          runtime.lib, program, host, dpi_inputs);
      !r) {
    return std::unexpected(std::move(r.error()));
  }
  auto exit_or = support::RunProcessStreaming(program, child_args);
  if (!exit_or) {
    return IoError(std::move(exit_or.error()));
  }
  return *exit_or;
}

}  // namespace lyra::driver
