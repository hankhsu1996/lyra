#include "lyra/driver/cpp_build.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <iterator>
#include <optional>
#include <set>
#include <span>
#include <sstream>
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
#include "lyra/stable_hash/fingerprint.hpp"
#include "lyra/status/status.hpp"
#include "lyra/support/runtime_prelude.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::driver {

namespace {

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
// having asked to skip it, `sha1sum` being available (used to name a prepared
// header by every file it reads, so an edit to any of them produces a
// different cache file rather than reusing one built against what is no longer
// on disk), and the compiler preparing an empty header at all.
// Any failing check falls back to plain compilation; correctness is
// unaffected.
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
# reused on later rebuilds; --no-pch skips it. It lands inside the
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
if [ "$NO_PCH" = "0" ] && command -v sha1sum >/dev/null 2>&1; then
  # Whether this compiler prepares a header, and which of the flags a prepared
  # header is better for it takes, each asked alone of an empty header: what a
  # command printed counts against it, so a flag warned of and ignored is not
  # taken. A flag goes on the command preparing the header, on every compile
  # against it, or on both, as its line says.
  P=@OBJDIR@/probe
  mkdir -p "$P"
  : > "$P/probe.hpp"
  takes() {
    OUT=$("$CXX" @STD@ $1 -xc++-header "$P/probe.hpp" -o "$P/probe.hpp@GCH@" \
      </dev/null 2>&1) &&
      [ -z "$OUT" ]
  }
  if takes ""; then
    USE_PCH=1
    PREPARE=""
    COMPILE=""
    while IFS='|' read -r flag reach; do
      if takes "$flag"; then
        case "$reach" in prepare|both) PREPARE="$PREPARE $flag" ;; esac
        case "$reach" in compile|both) COMPILE="$COMPILE $flag" ;; esac
      fi
    done <<FLAGS
@PCHFLAGS@
FLAGS
  fi
fi
PCH_FLAG=""
if [ "$USE_PCH" = "1" ]; then
  PRELUDE="@INCLUDE@/@PRELUDE@"
  PREP="@DESIGN_FLAGS@ $PREPARE"
  # A prepared header is named by everything that decided its contents, and by
  # nothing else: every file the prelude reads as this compiler reads it -- the
  # C++ library's and the system's as much as the runtime's, since a compiler
  # need not check those for itself -- and the command line below that says
  # what goes into it. A list that cannot be read whole prepares nothing.
  if DEPS=$("$CXX" @DESIGN_FLAGS@ -I @INCLUDE@ -M -MT prelude -xc++-header "$PRELUDE" 2>/dev/null) &&
    SUMS=$(printf '%s\n' "$DEPS" | sed -e 's/\\$//' -e 's/^prelude://' | tr ' ' '\n' |
      sed '/^$/d' | sort -u | xargs sha1sum 2>/dev/null); then
    FP=$( { printf '%s\n' "$SUMS"; echo "$PREP"; } | sha1sum | cut -c1-16)
    PCH="@CACHE@/prelude-${FP}"
    if [ ! -f "$PCH" ]; then
      mkdir -p "$(dirname "$PCH")"
      # Preparing one is an attempt like using one: a build that cannot have
      # the fast path takes the plain one instead of stopping, and a
      # half-written file goes so that the next build does not find it and
      # believe it.
      if ! "$CXX" $PREP -I @INCLUDE@ -xc++-header "$PRELUDE" -o "$PCH" 2>/dev/null; then
        rm -f "$PCH"
        USE_PCH=0
      fi
    fi
  else
    USE_PCH=0
  fi
  # Every compile is told to include a header holding one include of the
  # prelude, and finds the prepared header beside it under the name a compiler
  # looks for there; one that cannot use it refuses it, and the build compiles
  # again without it. Laying this out is an attempt too.
  if [ "$USE_PCH" = "1" ]; then
    if mkdir -p "$(dirname @OFFER@)" 2>/dev/null &&
      printf '#include "%s"\n' "@PRELUDE@" > @OFFER@ 2>/dev/null &&
      ln -sf "$(pwd)/$PCH" @OFFER@@GCH@ 2>/dev/null; then
      PCH_FLAG="-include @OFFER@ $COMPILE"
    else
      USE_PCH=0
    fi
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
    "$CXX" @DESIGN_FLAGS@ -I @INCLUDE@ $1 -c "$src" -o "@OBJDIR@/$src.o" \
      >>"$LOG" 2>&1 &
    PIDS="$PIDS $!"
    LIVE=$((LIVE + 1))
    if [ "$LIVE" -ge "$JOBS" ]; then reap; fi
  done
  reap
}
# Compiles the runtime's own sources, which the project carries where it is
# optimized with them at the link or checked for a sanitizer, each told what the
# runtime's build states for it and $1; RT_OBJS names the objects.
compile_runtime() {
  RT=@RTSRC@
  RT_FLAGS=""
  while read -r flag dir; do RT_FLAGS="$RT_FLAGS $flag $RT/$dir"; done < "$RT/@RTINCS@"
  while read -r name; do RT_FLAGS="$RT_FLAGS -D$name"; done < "$RT/@RTDEFS@"
  mkdir -p @OBJDIR@/runtime
  RT_OBJS=""
  PIDS=""
  LIVE=0
  n=0
  # Each line is a source and the macros its own library defines for it.
  while read -r src defs; do
    n=$((n + 1))
    RT_OBJS="$RT_OBJS @OBJDIR@/runtime/$n.o"
    case "$src" in
      *.c) SRC_FLAGS="@C_FLAGS@" ;;
      *) SRC_FLAGS="@DESIGN_FLAGS@" ;;
    esac
    for name in $defs; do SRC_FLAGS="$SRC_FLAGS -D$name"; done
    "$CXX" $SRC_FLAGS $RT_FLAGS $1 -c "$RT/$src" -o "@OBJDIR@/runtime/$n.o" \
      </dev/null >>"$LOG" 2>&1 &
    PIDS="$PIDS $!"
    LIVE=$((LIVE + 1))
    if [ "$LIVE" -ge "$JOBS" ]; then reap; fi
  done < "$RT/@RTLIST@"
  reap
}
# A project built to optimize at the link optimizes the design together with
# the runtime, whose sources it carries, in the first way this compiler and the
# linker it drives accept; a small program built each way in turn says which.
# Where none is, the program is built the plain way, with the prebuilt runtime.
# Once one is, the program is built that way, and a compile or link that then
# fails is the build's failure.
LTO=""
if [ "@LTO@" = "1" ]; then
  P=@OBJDIR@/probe
  mkdir -p "$P"
  printf '%s' '@PROBE@' > "$P/probe.cpp"
  while IFS='|' read -r compile cache; do
    if "$CXX" $compile -O2 -c "$P/probe.cpp" -o "$P/probe.o" 2>/dev/null &&
      "$CXX" $compile ${cache:+${cache}$P/cache} "$P/probe.o" -o "$P/probe" \
        2>/dev/null; then
      LTO="$compile"
      LTO_LINK="$compile${cache:+ ${cache}@OBJDIR@/lto}"
      break
    fi
  done <<FORMS
@LTOFORMS@
FORMS
fi
compile_all "$PCH_FLAG $LTO"
if [ "$STATUS" -ne 0 ] && [ "$USE_PCH" = "1" ]; then
  # The precompiled header is an attempt and never a requirement, so it does not
  # get to decide whether this build succeeds. Compile again the way that needs
  # nothing prepared; if that works, the header was refused rather than the
  # sources, and it goes so the next build prepares it afresh. Everything is
  # compiled again rather than only what failed, because tracking that costs
  # more shell than the rare path is worth.
  compile_all "$LTO"
  if [ "$STATUS" -eq 0 ]; then rm -f "$PCH"; fi
fi
# The program links the runtime compiled from its sources where it is optimized
# with it at the link, or checked for a sanitizer, since the library beside the
# project was compiled by another compiler.
RUNTIME=@LIBDIR@/@LIB@
if [ "$STATUS" -eq 0 ] && { [ -n "$LTO" ] || [ "@SANITIZE@" = "1" ]; }; then
  compile_runtime "$LTO"
  RUNTIME="$RT_OBJS"
fi
# What the compiler said is kept back until the build is known to have failed,
# so that an attempt which was retried and succeeded says nothing at all. What
# gets shown is the last attempt, which is the one that needed nothing prepared
# and so describes the sources rather than the header.
if [ "$STATUS" -ne 0 ]; then cat "$LOG" >&2; exit 1; fi
@DPICOMPILE@if [ -n "$LTO" ]; then
  "$CXX" @OPT@ @SANLINK@ $LTO_LINK$OBJS $RUNTIME@DPIOBJS@ -o @PROG@
else
  "$CXX" @SANLINK@$OBJS@DPIOBJS@ $RUNTIME -o @PROG@
fi
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
    std::span<const DpiLinkInput> inputs, const HostBuild& host) -> DpiRecipe {
  DpiRecipe recipe;
  for (const DpiLinkInput& input : inputs) {
    const std::string object = DpiObjectRelPath(input);
    recipe.compile_steps += std::format(
        "mkdir -p {}/{}\n\"$CXX\" {} -c {} -I . -o {}\n", kObjectDir,
        kDpiSourceDir,
        JoinWords(
            ForeignCompileFlags(input, host.optimization, host.sanitizer)),
        DpiSourceRelPath(input), object);
    recipe.link_inputs += std::format(" {}", object);
  }
  return recipe;
}

// Each way of optimizing at the link as a line the recipe reads in order: what
// a compile is told, then what the link is told before its cache directory,
// which is nothing where the form names no cache.
auto RenderLinkTimeForms() -> std::string {
  std::string lines;
  for (const LinkTimeForm& form : kLinkTimeForms) {
    if (!lines.empty()) lines += "\n";
    lines += std::format(
        "{}|{}", form.compile, form.cache ? form.cache->spelling : "");
  }
  return lines;
}

// Each flag a prepared header is better for as a line the recipe reads: the
// flag, then which command lines it goes on.
auto RenderOptionalFlags() -> std::string {
  std::string lines;
  for (const pch::OptionalFlag& optional : pch::kOptionalFlags) {
    if (!lines.empty()) lines += "\n";
    const std::string_view reach = [&] {
      switch (optional.reach) {
        case pch::FlagReach::kPreparing:
          return "prepare";
        case pch::FlagReach::kCompiling:
          return "compile";
        case pch::FlagReach::kPreparingAndCompiling:
          return "both";
      }
      std::unreachable();
    }();
    lines += std::format("{}|{}", optional.flag, reach);
  }
  return lines;
}

auto RenderBuildScript(
    const HostBuild& host, std::span<const DpiLinkInput> dpi_inputs,
    std::span<const TranslationUnit> translation_units) -> std::string {
  const std::string_view optimization_flag =
      OptimizationFlag(host.optimization);
  const DpiRecipe dpi = RenderDpiRecipe(dpi_inputs, host);
  std::vector<std::string> relpaths;
  relpaths.reserve(translation_units.size());
  for (const TranslationUnit& unit : translation_units) {
    relpaths.push_back(unit.relpath);
  }
  const std::string sources = JoinWords(relpaths);
  // Named locals, because the bindings below hold `string_view`s and are read
  // after this statement: a temporary would already have died.
  const std::string cxx_exe = host.cxx.string();
  const std::string design_flags =
      JoinWords(DesignCompileFlags(host.optimization, host.sanitizer));
  const std::string c_flags =
      JoinWords(RuntimeCCompileFlags(host.optimization, host.sanitizer));
  const std::string sanitizer_link =
      JoinWords(SanitizerLinkFlags(host.sanitizer));
  // Whether the project checks the program for a sanitizer.
  const std::string_view sanitize = [&] {
    switch (host.sanitizer) {
      case support::Sanitizer::kNone:
        return "0";
      case support::Sanitizer::kAddress:
        return "1";
    }
    std::unreachable();
  }();
  // Whether the project optimizes the design together with the runtime.
  const std::string_view lto = [&] {
    switch (host.link_time) {
      case LinkTimeOptimization::kNone:
        return "0";
      case LinkTimeOptimization::kThin:
        return "1";
    }
    std::unreachable();
  }();
  const std::string link_time_forms = RenderLinkTimeForms();
  const std::string optional_flags = RenderOptionalFlags();
  const std::array<std::pair<std::string_view, std::string_view>, 27> bindings =
      {{
          {"@INCLUDE@", kRuntimeIncludeDir},
          {"@PRELUDE@", support::kRuntimePreludeHeader},
          {"@CACHE@", kRuntimeCacheDir},
          {"@OBJDIR@", kObjectDir},
          {"@DESIGN_FLAGS@", design_flags},
          {"@C_FLAGS@", c_flags},
          {"@STD@", kCxxStandardFlag},
          {"@PCHFLAGS@", optional_flags},
          {"@OFFER@", kPreludeOfferHeader},
          {"@GCH@", kPreparedHeaderSuffix},
          {"@OPT@", optimization_flag},
          {"@CXX@", cxx_exe},
          {"@SOURCES@", sources},
          {"@LIBDIR@", kRuntimeLibDir},
          {"@LIB@", kRuntimeLibFile},
          {"@PROG@", kProgramName},
          {"@DPICOMPILE@", dpi.compile_steps},
          {"@DPIOBJS@", dpi.link_inputs},
          {"@LTO@", lto},
          {"@RTSRC@", kRuntimeSourceDir},
          {"@RTLIST@", kRuntimeSourceList},
          {"@RTINCS@", kRuntimeIncludeDirList},
          {"@RTDEFS@", kRuntimeDefineList},
          {"@SANITIZE@", sanitize},
          {"@SANLINK@", sanitizer_link},
          {"@LTOFORMS@", link_time_forms},
          {"@PROBE@", kLinkTimeProbe},
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
    return std::unexpected(
        diag::Make(
            diag::DiagCode::kHostIoError,
            "'clang-format' failed on the emitted sources")
            .WithNote("{}", run->stderr_text));
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

// Everything the compiler is told when a translation unit is compiled, short
// of the prepared header it may be offered and the file names. One list, so the
// command that compiles a file and the action it is kept under cannot describe
// different things.
auto CompileFlags(
    const std::filesystem::path& include_root, OptimizationLevel optimization,
    support::Sanitizer sanitizer) -> std::vector<std::string> {
  std::vector<std::string> flags = DesignCompileFlags(optimization, sanitizer);
  flags.emplace_back("-I");
  flags.push_back(include_root.string());
  return flags;
}

// The action compiling `unit`: the file, every file of the project it
// includes, each under its path, and what else the compiler reads or is told.
// A prepared header is no input, since nothing a compile produces may depend on
// whether it was offered one.
auto CompileFile(
    const std::filesystem::path& dir, const TranslationUnit& unit,
    const stable_hash::Fingerprint& prelude_inputs,
    const std::filesystem::path& cxx, std::span<const std::string> flags)
    -> Action {
  Action action("compile C++ file");
  action.InputFile("source", dir / unit.relpath);
  for (const std::string& included : unit.includes) {
    action.Input("included", included);
    action.InputFile("content", dir / included);
  }
  action.InputDigest("prelude inputs", prelude_inputs);
  action.InputExecutable("compiler", cxx);
  for (const std::string& flag : flags) {
    action.Input("flag", flag);
  }
  return action;
}

// Compiles `source` to `object`. A header compiled in advance may not decide
// whether a build succeeds, so a compile that failed with one is run again
// without it before its output counts as a failure; one that then succeeds was
// refused the header rather than the source, and the header is dropped so the
// next build prepares a fresh one.
auto CompileOne(
    const std::filesystem::path& source, const std::filesystem::path& object,
    const std::filesystem::path& cxx, std::span<const std::string> flags,
    const std::optional<pch::Offer>& prelude, const ActionCache& actions)
    -> diag::Result<void> {
  const auto run = [&](const pch::Offer* offer) {
    std::vector<std::string> args(flags.begin(), flags.end());
    if (offer != nullptr) {
      args.insert(args.end(), offer->flags.begin(), offer->flags.end());
    }
    args.emplace_back("-c");
    args.push_back(source.string());
    args.emplace_back("-o");
    args.push_back(object.string());
    return RunProcessCaptured(cxx, args);
  };
  if (prelude.has_value()) {
    auto fast = run(&*prelude);
    if (!fast) {
      return diag::Fail(diag::DiagCode::kHostIoError, std::move(fast.error()));
    }
    if (fast->exit_code == 0) {
      return {};
    }
  }
  auto plain = run(nullptr);
  if (!plain) {
    return diag::Fail(diag::DiagCode::kHostIoError, std::move(plain.error()));
  }
  if (plain->exit_code != 0) {
    return std::unexpected(
        diag::Make(
            diag::DiagCode::kHostBuildFailed, "compiling '{}' failed",
            source.filename().string())
            .WithNote("{}", plain->stderr_text));
  }
  if (prelude.has_value()) {
    actions.Evict(prelude->prepared);
  }
  return {};
}

// Runs `count` compiles, `width` at once, answering each one's output in
// order. Seeing every file that does not compile at once saves the run it would
// otherwise take to find the next, so the first failure carries every later one
// as notes of its own.
template <typename Compile>
auto CompileEach(std::size_t count, std::size_t width, Compile compile)
    -> diag::Result<std::vector<ActionResult>> {
  std::vector<ActionResult> compiled;
  compiled.reserve(count);
  std::optional<diag::Diagnostic> failures;
  base::ProduceInOrder(
      count, width, compile, [&](diag::Result<ActionResult> result) {
        if (!result) {
          if (!failures) {
            failures = std::move(result.error());
            return;
          }
          failures->notes.push_back(
              diag::NoteDiagItem{
                  .span = result.error().primary.span,
                  .message = std::move(result.error().primary.message)});
          std::ranges::move(
              result.error().notes, std::back_inserter(failures->notes));
          return;
        }
        compiled.push_back(*std::move(result));
      });
  if (failures) {
    return std::unexpected(*std::move(failures));
  }
  return compiled;
}

// One source the runtime carries, and what its own library's build adds to
// every compile of it.
struct RuntimeSource {
  std::filesystem::path path;
  std::vector<std::string> flags;
};

// The runtime's own sources and what the runtime's build states each compile
// is told, read from the lists staged beside them, each path made absolute.
struct RuntimeSources {
  std::vector<RuntimeSource> translation_units;
  std::vector<std::string> flags;
};

auto ReadLines(const std::filesystem::path& file)
    -> diag::Result<std::vector<std::string>> {
  std::ifstream in(file);
  if (!in) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot read '{}'", file.string());
  }
  std::vector<std::string> lines;
  for (std::string line; std::getline(in, line);) {
    if (!line.empty()) {
      lines.push_back(std::move(line));
    }
  }
  return lines;
}

auto ReadRuntimeSources(const std::filesystem::path& root)
    -> diag::Result<RuntimeSources> {
  auto sources = ReadLines(root / kRuntimeSourceList);
  if (!sources) {
    return std::unexpected(std::move(sources.error()));
  }
  auto include_dirs = ReadLines(root / kRuntimeIncludeDirList);
  if (!include_dirs) {
    return std::unexpected(std::move(include_dirs.error()));
  }
  auto defines = ReadLines(root / kRuntimeDefineList);
  if (!defines) {
    return std::unexpected(std::move(defines.error()));
  }
  RuntimeSources read;
  for (const std::string& line : *sources) {
    // Each line is a path and the macros its library defines for it alone.
    std::istringstream words(line);
    std::string path;
    words >> path;
    RuntimeSource source{.path = root / path, .flags = {}};
    for (std::string define; words >> define;) {
      source.flags.push_back("-D" + define);
    }
    read.translation_units.push_back(std::move(source));
  }
  for (const std::string& line : *include_dirs) {
    // Each line is a flag and the directory it names.
    const std::size_t space = line.find(' ');
    if (space == std::string::npos) {
      throw InternalError(
          std::format("the runtime's include list holds '{}'", line));
    }
    read.flags.push_back(line.substr(0, space));
    read.flags.push_back((root / line.substr(space + 1)).string());
  }
  for (const std::string& define : *defines) {
    read.flags.push_back("-D" + define);
  }
  return read;
}

// The files a make rule names after its target's colon, as a compiler asked
// for its dependencies writes one: separated by blanks, continued over lines
// by a backslash ending one, with a blank inside a name escaped by a backslash
// and a dollar sign doubled.
auto PrerequisitesOf(std::string_view rule)
    -> std::vector<std::filesystem::path> {
  const std::size_t colon = rule.find(':');
  std::vector<std::filesystem::path> files;
  if (colon == std::string_view::npos) {
    return files;
  }
  std::string name;
  const auto close = [&] {
    if (!name.empty()) {
      files.emplace_back(std::move(name));
      name.clear();
    }
  };
  for (std::size_t i = colon + 1; i < rule.size(); ++i) {
    const char c = rule[i];
    const char next = i + 1 < rule.size() ? rule[i + 1] : '\0';
    if (c == '\\' && (next == '\n' || next == '\r')) {
      close();
      ++i;
    } else if (c == '\\' && next == ' ') {
      name += ' ';
      ++i;
    } else if (c == '$' && next == '$') {
      name += '$';
      ++i;
    } else if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
      close();
    } else {
      name += c;
    }
  }
  close();
  return files;
}

}  // namespace

auto FindUsualCompiler() -> std::optional<std::filesystem::path> {
  for (const std::string_view usual : {"clang++", "c++"}) {
    if (auto found = FindOnPath(usual)) {
      return *std::move(found);
    }
  }
  return std::nullopt;
}

auto DigestOfPreludeInputs(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    OptimizationLevel optimization, support::Sanitizer sanitizer)
    -> diag::Result<stable_hash::Fingerprint> {
  std::vector<std::string> args =
      CompileFlags(include_root, optimization, sanitizer);
  args.insert(
      args.end(), {"-M", "-MT", "prelude", "-xc++-header",
                   (include_root / support::kRuntimePreludeHeader).string()});
  auto listed = RunProcessCaptured(cxx, args);
  if (!listed) {
    return diag::Fail(diag::DiagCode::kHostIoError, std::move(listed.error()));
  }
  if (listed->exit_code != 0) {
    return std::unexpected(
        diag::Make(
            diag::DiagCode::kHostBuildFailed,
            "the runtime's headers do not compile with '{}'", cxx.string())
            .WithNote("{}", listed->stderr_text));
  }
  return DigestOfFiles(include_root, PrerequisitesOf(listed->stdout_text));
}

auto CompileTranslationUnits(
    const std::filesystem::path& dir,
    std::span<const TranslationUnit> translation_units,
    const RuntimeLocation& runtime, const HostBuild& host,
    std::optional<std::string_view> link_time, ActionCache& actions)
    -> diag::Result<std::vector<ActionResult>> {
  const auto prelude_inputs = DigestOfPreludeInputs(
      host.cxx, runtime.include_root, host.optimization, host.sanitizer);
  if (!prelude_inputs) {
    return std::unexpected(prelude_inputs.error());
  }
  std::vector<std::string> flags =
      CompileFlags(runtime.include_root, host.optimization, host.sanitizer);
  if (link_time) {
    flags.emplace_back(*link_time);
  }
  // The prelude is compiled before anything reads it, so several compiles
  // cannot each find it missing and race to build the same file.
  const std::optional<pch::Offer> prelude = pch::OfferPrelude(
      host.cxx, runtime.include_root, *prelude_inputs, host.pch, actions,
      host.optimization, host.sanitizer, dir);

  const status::Phase phase("Compiling C++");
  status::Pieces(translation_units.size());
  return CompileEach(
      translation_units.size(), host.compile_width,
      [&](std::size_t i) -> diag::Result<ActionResult> {
        const TranslationUnit& unit = translation_units[i];
        const status::Piece piece(unit.source_name);
        return actions.Execute(
            CompileFile(dir, unit, *prelude_inputs, host.cxx, flags),
            [&](const std::filesystem::path& object) {
              return CompileOne(
                  dir / unit.relpath, object, host.cxx, flags, prelude,
                  actions);
            });
      });
}

auto LinkFlagsFor(
    const LinkTimeForm& form, OptimizationLevel level,
    const std::optional<std::filesystem::path>& cache)
    -> std::vector<std::string> {
  std::vector<std::string> flags{
      std::string(form.compile), std::string(OptimizationFlag(level))};
  if (form.cache && cache) {
    flags.push_back(std::string(form.cache->spelling) + cache->string());
  }
  return flags;
}

auto AcceptedLinkTimeForm(const HostBuild& host, ActionCache& actions)
    -> std::optional<LinkTimeForm> {
  Action action("find how the host compiler optimizes at the link");
  action.InputExecutable("compiler", host.cxx);
  action.Input("probe", kLinkTimeProbe);
  for (const LinkTimeForm& form : kLinkTimeForms) {
    action.Input("compile", form.compile);
    action.Input("cache", form.cache ? form.cache->spelling : "");
  }
  const status::Phase phase("Checking link-time optimization");
  // The output names the accepted form by its place in the list, and is empty
  // where none is accepted.
  auto probed = actions.Execute(
      action, [&](const std::filesystem::path& output) -> diag::Result<void> {
        std::filesystem::path dir = output;
        dir += ".probe";
        std::error_code ec;
        std::filesystem::create_directories(dir, ec);
        if (ec) {
          return diag::Fail(
              diag::DiagCode::kHostIoError, "cannot create '{}' ({})",
              dir.string(), ec.message());
        }
        const std::filesystem::path source = dir / "probe.cpp";
        const std::filesystem::path object = dir / "probe.o";
        if (auto written = WriteFile(source, kLinkTimeProbe); !written) {
          return written;
        }
        // Whether the probe compiles and links under `form`.
        const auto accepts = [&](const LinkTimeForm& form) {
          const auto succeeds = [&](const std::vector<std::string>& args) {
            const auto run = RunProcessCaptured(host.cxx, args);
            return run && run->exit_code == 0;
          };
          std::vector<std::string> link =
              LinkFlagsFor(form, OptimizationLevel::kO2, dir / "cache");
          link.push_back(object.string());
          link.emplace_back("-o");
          link.push_back((dir / "probe").string());
          return succeeds(
                     {std::string(form.compile),
                      std::string(OptimizationFlag(OptimizationLevel::kO2)),
                      "-c", source.string(), "-o", object.string()}) &&
                 succeeds(link);
        };
        const auto* const first = std::ranges::find_if(kLinkTimeForms, accepts);
        return WriteFile(
            output,
            first == kLinkTimeForms.end()
                ? std::string()
                : std::format(
                      "{}", std::distance(kLinkTimeForms.begin(), first)));
      });
  if (!probed) {
    return std::nullopt;
  }
  std::ifstream in(probed->path);
  std::size_t index = 0;
  if (!(in >> index) || index >= kLinkTimeForms.size()) {
    return std::nullopt;
  }
  return kLinkTimeForms.at(index);
}

auto CompileRuntimeSources(
    const RuntimeLocation& runtime, const HostBuild& host,
    std::optional<std::string_view> link_time, ActionCache& actions)
    -> diag::Result<std::vector<ActionResult>> {
  auto sources = ReadRuntimeSources(runtime.sources_root);
  if (!sources) {
    return std::unexpected(std::move(sources.error()));
  }
  const stable_hash::Fingerprint tree = DigestOfTree(runtime.sources_root);

  const status::Phase phase("Compiling the runtime");
  status::Pieces(sources->translation_units.size());
  return CompileEach(
      sources->translation_units.size(), host.compile_width,
      [&](std::size_t i) -> diag::Result<ActionResult> {
        const RuntimeSource& unit = sources->translation_units[i];
        const std::filesystem::path& source = unit.path;
        std::vector<std::string> flags =
            source.extension() == ".c"
                ? RuntimeCCompileFlags(host.optimization, host.sanitizer)
                : DesignCompileFlags(host.optimization, host.sanitizer);
        flags.insert(flags.end(), sources->flags.begin(), sources->flags.end());
        flags.insert(flags.end(), unit.flags.begin(), unit.flags.end());
        if (link_time) {
          flags.emplace_back(*link_time);
        }
        const status::Piece piece(source.filename().string());
        Action action("compile runtime source");
        action.Input(
            "source", source.lexically_relative(runtime.sources_root).string());
        action.InputDigest("source tree", tree);
        action.InputExecutable("compiler", host.cxx);
        for (const std::string& flag : flags) {
          action.Input("flag", flag);
        }
        return actions.Execute(
            action, [&](const std::filesystem::path& object) {
              return CompileOne(
                  source, object, host.cxx, flags, std::nullopt, actions);
            });
      });
}

auto LinkProgram(
    std::span<const std::filesystem::path> inputs,
    std::span<const std::string> flags, const std::filesystem::path& program,
    const std::filesystem::path& cxx) -> diag::Result<void> {
  std::vector<std::string> args(flags.begin(), flags.end());
  for (const std::filesystem::path& input : inputs) {
    args.push_back(input.string());
  }
  args.emplace_back("-o");
  args.push_back(program.string());
  const status::Phase phase("Linking");
  auto result_or = RunProcessCaptured(cxx, args);
  if (!result_or) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, std::move(result_or.error()));
  }
  if (result_or->exit_code != 0) {
    return std::unexpected(
        diag::Make(
            diag::DiagCode::kHostBuildFailed, "linking the program failed")
            .WithNote("{}", result_or->stderr_text));
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
          {.relpath = artifacts.code.relpath,
           .source_name = unit.source_name,
           .includes = artifacts.code.includes},
      .dpi_fragment = dpi::AbiFragmentOf(unit),
      .declarations = artifacts.declarations};
}

void CppProjectSink::WriteDeclarations(WrittenUnit& written) {
  std::vector<support::Artifact> recorded;
  for (const backend::cpp::CppArtifact& declaration : written.declarations) {
    const stable_hash::Fingerprint says =
        stable_hash::FingerprintOfText(declaration.content);
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
    includes_of_.emplace(declaration.relpath, declaration.includes);
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
          .translation_unit =
              {.relpath = host_main.relpath,
               .source_name = {},
               .includes = host_main.includes},
          .dpi_fragment = std::nullopt,
          .declarations = {}});
  if (formatting_ == SourceFormatting::kOn) {
    return FormatSources(written_, dir_);
  }
  return {};
}

auto CppProjectSink::TakeSources() -> EmittedCppSources {
  for (TranslationUnit& unit : translation_units_) {
    std::set<std::string> reached;
    std::vector<std::string> pending = std::move(unit.includes);
    while (!pending.empty()) {
      std::string file = std::move(pending.back());
      pending.pop_back();
      if (!reached.insert(file).second) continue;
      const auto found = includes_of_.find(file);
      if (found == includes_of_.end()) {
        throw InternalError(
            std::format(
                "backend::cpp: '{}' includes '{}', which the project does not "
                "hold -- please report this as a bug",
                unit.relpath, file));
      }
      pending.insert(pending.end(), found->second.begin(), found->second.end());
    }
    unit.includes.assign(reached.begin(), reached.end());
  }
  return EmittedCppSources{
      .translation_units = std::move(translation_units_),
      .dpi_fragments = std::move(dpi_fragments_)};
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
          script_path,
          RenderBuildScript(host, dpi_inputs, sources.translation_units));
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
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot make '{}' executable ({})",
        script_path.string(), ec.message());
  }

  return ExportRuntimeTree(runtime, dir, host.link_time, host.sanitizer);
}

}  // namespace lyra::driver
