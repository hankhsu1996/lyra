#include "lyra/driver/cpp_build.hpp"

#include <array>
#include <cstddef>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iterator>
#include <optional>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/parallel.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/driver/action_cache.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/driver/file_output.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/driver/subprocess.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/status/status.hpp"
#include "lyra/support/runtime_prelude.hpp"
#include "lyra/support/statistics.hpp"

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
# A precompiled header carrying the runtime surface is built on first run and
# reused on later rebuilds (clang only); --no-pch skips it. It lands inside the
# project and serves that project alone: what it holds is bound to the paths of
# the headers it was prepared from, and every project carries its own copy of
# those, which is what lets a directory be moved to another machine.
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
  # A prepared header is named by everything that decided its contents, and by
  # nothing else: the headers it was built from, and the command line below that
  # says what goes into it. Re-emitting into a directory that already holds one
  # then cannot reuse a header prepared a different way from the same headers.
  PREP="@STD@ @OPT@ @VALIDATE@ @PREPARE@"
  FP=$( { find @INCLUDE@ -name '*.hpp' -print0 | sort -z | xargs -0 sha1sum; echo "$PREP"; } | sha1sum | cut -c1-16)
  PCH="@CACHE@/prelude-${FP}-@OPTTAG@.pch"
  if [ ! -f "$PCH" ]; then
    mkdir -p "$(dirname "$PCH")"
    # Preparing one is an attempt like using one: a build that cannot have the
    # fast path takes the plain one instead of stopping, and a half-written file
    # goes so that the next build does not find it and believe it.
    if ! "$CXX" $PREP -I @INCLUDE@ -xc++-header "$PRELUDE" -o "$PCH" 2>/dev/null; then
      rm -f "$PCH"
      USE_PCH=0
    fi
  fi
  if [ "$USE_PCH" = "1" ]; then
    PCH_FLAG="-include-pch $PCH @VALIDATE@"
  fi
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
    std::span<const TranslationUnit> translation_units,
    Optimization optimization) -> std::string {
  const std::string_view optimization_flag = OptimizationFlag(optimization);
  const DpiRecipe dpi = RenderDpiRecipe(dpi_inputs, optimization_flag);
  std::vector<std::string> relpaths;
  relpaths.reserve(translation_units.size());
  for (const TranslationUnit& unit : translation_units) {
    relpaths.push_back(unit.relpath);
  }
  const std::string sources = JoinWords(relpaths);
  // Named locals, because the bindings below hold `string_view`s and are read
  // after this statement: a temporary would already have died.
  const std::string cxx_exe = cxx.string();
  // The recipe keys its own PCH cache by header content, which does not
  // separate two builds clang will refuse to share.
  const std::string_view optimization_tag = optimization_flag.substr(1);
  const std::array<std::pair<std::string_view, std::string_view>, 16> bindings =
      {{
          {"@INCLUDE@", kRuntimeIncludeDir},
          {"@PRELUDE@", support::kRuntimePreludeHeader},
          {"@CACHE@", kRuntimeCacheDir},
          {"@OBJDIR@", kObjectDir},
          {"@STD@", kCxxStandardFlag},
          {"@OPT@", optimization_flag},
          {"@VALIDATE@", kPchContentValidationFlag},
          {"@PREPARE@", kPchInstantiateTemplatesFlag},
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
  auto clang_format = FindOnPath("clang-format");
  if (!clang_format) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, std::move(clang_format.error()));
  }
  std::vector<std::string> args = {"-i", "-style=Google"};
  for (const std::string& relpath : relpaths) {
    args.push_back((dir / relpath).string());
  }
  const status::Phase phase("Formatting");
  auto run = RunProcessCaptured(*clang_format, args);
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

// Every file of the project in `dir` that `source` reaches through an
// `#include "..."`, each once and in a fixed order: what the compiler reads of
// the project beside the file itself. A name the project does not hold is the
// runtime's, whose headers are an input of their own.
auto ProjectIncludesOf(
    const std::filesystem::path& dir, const std::string& source)
    -> std::vector<std::string> {
  constexpr std::string_view kInclude = "#include \"";
  std::set<std::string> reached;
  std::vector<std::string> pending = {source};
  while (!pending.empty()) {
    std::ifstream in(dir / pending.back());
    pending.pop_back();
    std::string line;
    while (std::getline(in, line)) {
      const std::size_t end = line.find('"', kInclude.size());
      if (!line.starts_with(kInclude) || end == std::string::npos) {
        continue;
      }
      std::string included =
          line.substr(kInclude.size(), end - kInclude.size());
      std::error_code ec;
      if (std::filesystem::is_regular_file(dir / included, ec) &&
          reached.insert(included).second) {
        pending.push_back(std::move(included));
      }
    }
  }
  return {reached.begin(), reached.end()};
}

// The action compiling `source`: the file, every file of the project it
// includes, each under its path, and what else the compiler reads or is told.
// A prepared header is no input, since nothing a compile produces may depend on
// whether it was offered one.
auto CompileFile(
    const std::filesystem::path& dir, const std::string& source,
    const RuntimeLocation& runtime, const HostBuild& host) -> Action {
  Action action("compile C++ file");
  action.InputFile("source", dir / source);
  for (const std::string& included : ProjectIncludesOf(dir, source)) {
    action.Input("included", included);
    action.InputFile("content", dir / included);
  }
  action.InputTree("runtime headers", runtime.include_root);
  action.InputExecutable("compiler", host.cxx);
  action.Input("optimization", OptimizationFlag(host.optimization));
  return action;
}

// Compiles `source` to `object`. A header compiled in advance may not decide
// whether a build succeeds, so a compile that failed with one is run again
// without it before its output counts as a failure; one that then succeeds was
// refused the header rather than the source, and the header is dropped so the
// next build prepares a fresh one.
auto CompileOne(
    const std::filesystem::path& source, const std::filesystem::path& object,
    const std::filesystem::path& include_root, const HostBuild& host,
    const std::optional<std::filesystem::path>& prelude) -> diag::Result<void> {
  const auto run = [&](const std::optional<std::filesystem::path>& prepared) {
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
    args.push_back(source.string());
    args.emplace_back("-o");
    args.push_back(object.string());
    return RunProcessCaptured(host.cxx, args);
  };
  if (prelude.has_value()) {
    auto fast = run(prelude);
    if (!fast) {
      return IoError(std::move(fast.error()));
    }
    if (fast->exit_code == 0) {
      return {};
    }
  }
  auto plain = run(std::nullopt);
  if (!plain) {
    return IoError(std::move(plain.error()));
  }
  if (plain->exit_code != 0) {
    return diag::Fail(
        diag::DiagCode::kHostBuildFailed,
        std::format(
            "compiling '{}' failed:\n{}", source.filename().string(),
            plain->stderr_text));
  }
  if (prelude.has_value()) {
    Discard(*prelude);
  }
  return {};
}

}  // namespace

auto CompileTranslationUnits(
    const std::filesystem::path& dir,
    std::span<const TranslationUnit> translation_units,
    const RuntimeLocation& runtime, const HostBuild& host, ActionCache& actions)
    -> diag::Result<std::vector<ActionResult>> {
  // The prelude is compiled before anything reads it, so several compiles
  // cannot each find it missing and race to build the same file.
  const std::optional<std::filesystem::path> prelude = pch::EnsureCached(
      host.cxx, runtime.include_root, host.pch, actions, host.optimization);

  const status::Phase phase("Compiling C++");
  status::Pieces(translation_units.size());
  std::vector<ActionResult> compiled;
  compiled.reserve(translation_units.size());
  // A build whose emitted text does not compile is a defect in Lyra, and
  // seeing every such file at once is what saves the run it would otherwise
  // take to find the next.
  std::string failures;
  base::ProduceInOrder(
      translation_units.size(), host.compile_width,
      [&](std::size_t i) -> diag::Result<ActionResult> {
        const TranslationUnit& unit = translation_units[i];
        const status::Piece piece(unit.source_name);
        return actions.Execute(
            CompileFile(dir, unit.relpath, runtime, host),
            [&](const std::filesystem::path& object) {
              return CompileOne(
                  dir / unit.relpath, object, runtime.include_root, host,
                  prelude);
            });
      },
      [&](diag::Result<ActionResult> result) {
        if (!result) {
          failures += (failures.empty() ? "" : "\n");
          failures += result.error().primary.message;
          return;
        }
        compiled.push_back(*std::move(result));
      });
  if (!failures.empty()) {
    return diag::Fail(diag::DiagCode::kHostBuildFailed, std::move(failures));
  }
  return compiled;
}

auto LinkProgram(
    std::span<const std::filesystem::path> objects,
    const std::filesystem::path& runtime_lib,
    const std::filesystem::path& program, const std::filesystem::path& cxx)
    -> diag::Result<void> {
  std::vector<std::string> args;
  args.reserve(objects.size() + 3);
  for (const std::filesystem::path& object : objects) {
    args.push_back(object.string());
  }
  args.push_back(runtime_lib.string());
  args.emplace_back("-o");
  args.push_back(program.string());
  const status::Phase phase("Linking");
  auto result_or = RunProcessCaptured(cxx, args);
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

auto CppProjectSink::Write(const mir::CompilationUnit& unit) const
    -> diag::Result<EmittedUnit> {
  const profiling::TimeTraceScope span(
      "emit C++", [&] { return unit.body_name; });
  diag::DiagnosticSink refused;
  const backend::cpp::CppUnitArtifacts artifacts =
      backend::cpp::EmitCppUnit(unit, refused);
  if (refused.HasErrors()) {
    status::Errors(refused.Diagnostics().size());
    return RefusedUnit{.refusals = refused.Diagnostics()};
  }
  if (auto r = WriteArtifact(artifacts.code); !r) {
    return std::unexpected(std::move(r.error()));
  }
  support::RecordUnit(
      unit.body_name, {support::Artifact{
                          .kind = support::ArtifactKind::kCppSource,
                          .name = artifacts.code.relpath,
                          .bytes = artifacts.code.content.size(),
                          .made = true}});
  return WrittenUnit{
      .body_name = unit.body_name,
      .files = {artifacts.code.relpath},
      .translation_unit =
          {.relpath = artifacts.code.relpath, .source_name = unit.source_name},
      .dpi_fragment = dpi::AbiFragmentOf(unit),
      .declarations = artifacts.declarations};
}

void CppProjectSink::WriteDeclarations(WrittenUnit& written) {
  std::vector<support::Artifact> recorded;
  for (const backend::cpp::CppArtifact& declaration : written.declarations) {
    const std::size_t says = std::hash<std::string>{}(declaration.content);
    const auto [stated, first] =
        declared_.try_emplace(declaration.relpath, says);
    if (!first) {
      if (stated->second != says) {
        throw InternalError(
            std::format(
                "backend::cpp: '{}' states '{}' differently from another body "
                "of its unit -- please report this as a bug",
                written.body_name, declaration.relpath));
      }
      continue;
    }
    if (auto r = WriteArtifact(declaration); !r) {
      refusals_->Report(std::move(r.error()));
      continue;
    }
    written_.push_back(declaration.relpath);
    recorded.push_back(
        support::Artifact{
            .kind = support::ArtifactKind::kCppHeader,
            .name = declaration.relpath,
            .bytes = declaration.content.size(),
            .made = true});
  }
  support::RecordUnit(written.body_name, std::move(recorded));
}

void CppProjectSink::Collect(EmittedUnit unit) {
  std::visit(
      Overloaded{
          [&](WrittenUnit& written) {
            WriteDeclarations(written);
            written_.insert(
                written_.end(), std::make_move_iterator(written.files.begin()),
                std::make_move_iterator(written.files.end()));
            translation_units_.push_back(std::move(written.translation_unit));
            if (written.dpi_fragment.has_value()) {
              dpi::AddAbiFragment(
                  dpi_fragments_, *std::move(written.dpi_fragment));
            }
          },
          [&](RefusedUnit& refused) {
            for (diag::Diagnostic& refusal : refused.refusals) {
              refusals_->Report(std::move(refusal));
            }
          }},
      unit);
}

auto CppProjectSink::Finish(const mir::CompilationUnit& root)
    -> diag::Result<void> {
  auto written = Write(root);
  if (!written) {
    return std::unexpected(std::move(written.error()));
  }
  Collect(*std::move(written));
  if (refusals_->HasErrors()) {
    return {};
  }
  const backend::cpp::CppArtifact host_main =
      backend::cpp::EmitCppHostMain(root);
  if (auto r = WriteArtifact(host_main); !r) {
    return r;
  }
  Collect(
      WrittenUnit{
          .body_name = root.body_name,
          .files = {host_main.relpath},
          .translation_unit = {.relpath = host_main.relpath, .source_name = {}},
          .dpi_fragment = std::nullopt,
          .declarations = {}});
  if (formatting_ == SourceFormatting::kOn) {
    return FormatSources(written_, dir_);
  }
  return {};
}

auto CppProjectSink::WriteArtifact(const backend::cpp::CppArtifact& file) const
    -> diag::Result<void> {
  return WriteFile(dir_ / file.relpath, file.content);
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

}  // namespace lyra::driver
