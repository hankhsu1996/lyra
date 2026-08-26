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
// unchanged. The PCH section gates on three runtime checks: `$CXX` looking
// like clang (gcc's PCH dialect does not match these flags), `LYRA_NO_PCH`
// being unset, and `sha1sum` being available (used to fingerprint the
// header tree so a header edit produces a different cache file rather
// than reusing a PCH built against stale-on-disk content). Any failing
// check falls back to plain compilation; correctness is unaffected.
constexpr std::string_view kBuildScriptTemplate = R"sh(#!/bin/sh
# Build this self-contained Lyra C++ project.
#
#   usage: build.sh [--cxx <compiler>] [--no-pch]
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
# A precompiled header is built on first run and reused on later rebuilds to
# amortize parsing of the runtime headers (clang only); --no-pch skips it.
set -e
CXX="@CXX@"
NO_PCH=0
while [ $# -gt 0 ]; do
  case "$1" in
    --cxx)
      if [ $# -lt 2 ]; then echo "build.sh: --cxx needs a value" >&2; exit 2; fi
      CXX="$2"; shift 2 ;;
    --no-pch) NO_PCH=1; shift ;;
    *) echo "usage: build.sh [--cxx <compiler>] [--no-pch]" >&2; exit 2 ;;
  esac
done
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
    "$CXX" @STD@ @OPT@ -I @INCLUDE@ -xc++-header "$PRELUDE" -o "$PCH"
  fi
  PCH_FLAG="-include-pch $PCH"
fi
@DPICOMPILE@"$CXX" @STD@ @OPT@ -I @INCLUDE@ $PCH_FLAG @MAIN@@DPIOBJS@ @LIBDIR@/@LIB@ -o @PROG@
)sh";

// Where a DPI-C source sits once copied into the project, relative to it. The
// recipe and the copy read the location from here, so neither restates it.
auto DpiSourceRelPath(const DpiLinkInput& input) -> std::string {
  return std::format("{}/{}", kDpiSourceDir, input.source.filename().string());
}

// The build recipe's DPI-C contribution (LRM 35), rendered from the already
// classified link inputs so the script carries no language detection of its
// own: each C source gets its own compile step that keeps its symbols' C
// linkage, and everything else joins the C++ link line directly. Both halves
// are empty for a design with no foreign sources, which is why the recipe needs
// no branch for that case.
struct DpiRecipe {
  std::string compile_steps;
  std::string link_inputs;
};

auto RenderDpiRecipe(
    std::span<const DpiLinkInput> inputs, std::string_view optimization_flag)
    -> DpiRecipe {
  DpiRecipe recipe;
  for (const DpiLinkInput& input : inputs) {
    const std::string relative = DpiSourceRelPath(input);
    if (!input.compile_as_c) {
      recipe.link_inputs += std::format(" {}", relative);
      continue;
    }
    const std::string object = relative + ".o";
    recipe.compile_steps += std::format(
        "\"$CXX\" {} -x c -c {} -I . -o {}\n", optimization_flag, relative,
        object);
    recipe.link_inputs += std::format(" {}", object);
  }
  return recipe;
}

auto RenderBuildScript(
    const std::filesystem::path& cxx, std::span<const DpiLinkInput> dpi_inputs,
    Optimization optimization) -> std::string {
  const std::string_view optimization_flag = OptimizationFlag(optimization);
  const DpiRecipe dpi = RenderDpiRecipe(dpi_inputs, optimization_flag);
  // Named locals, because the bindings below hold `string_view`s and are read
  // after this statement: a temporary would already have died.
  const std::string cxx_exe = cxx.string();
  // The recipe keys its own PCH cache by header content, which does not
  // separate two builds clang will refuse to share.
  const std::string_view optimization_tag = optimization_flag.substr(1);
  const std::array<std::pair<std::string_view, std::string_view>, 13> bindings =
      {{
          {"@INCLUDE@", kRuntimeIncludeDir},
          {"@PRELUDE@", support::kRuntimePreludeHeader},
          {"@CACHE@", kRuntimeCacheDir},
          {"@STD@", kCxxStandardFlag},
          {"@OPT@", optimization_flag},
          {"@OPTTAG@", optimization_tag},
          {"@CXX@", cxx_exe},
          {"@MAIN@", kMainSource},
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
    std::span<const backend::cpp::CppArtifact> files,
    const std::filesystem::path& dir) -> diag::Result<void> {
  auto clang_format = support::FindOnPath("clang-format");
  if (!clang_format) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, std::move(clang_format.error()));
  }
  std::vector<std::string> args = {"-i", "-style=Google"};
  for (const auto& file : files) {
    args.push_back((dir / file.relpath).string());
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
    if (auto r = CopyFileWritable(input.source, dir / DpiSourceRelPath(input));
        !r) {
      return r;
    }
  }
  return {};
}

auto EmitAndWriteSources(
    std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root, const std::filesystem::path& dir,
    SourceFormatting formatting) -> diag::Result<void> {
  auto set = backend::cpp::EmitCpp(units, root);
  for (const auto& file : set.files) {
    if (auto r = WriteFile(dir / file.relpath, file.content); !r) {
      return r;
    }
  }
  if (formatting == SourceFormatting::kOn) {
    if (auto r = FormatSources(set.files, dir); !r) {
      return r;
    }
  }
  return {};
}

// Prepares one DPI-C link input for the final link (LRM 35). A C source is
// compiled to an object in its own step so its symbols keep C linkage -- the
// emitted declaration expects that, and the C++ driver would otherwise mangle a
// C source compiled in the C++ invocation. A C++ source joins the C++ link
// directly. `header_dir` holds the generated ABI header the source may include.
// Returns the path to add to the link line.
auto PrepareDpiLinkInput(
    const HostBuild& host, const DpiLinkInput& input,
    const std::filesystem::path& header_dir,
    const std::filesystem::path& work_dir) -> diag::Result<std::string> {
  const std::string source = input.source.string();
  if (!input.compile_as_c) {
    return source;
  }
  const std::filesystem::path obj =
      work_dir / (input.source.filename().string() + ".o");
  const std::vector<std::string> compile_args = {
      std::string(OptimizationFlag(host.optimization)),
      "-x",
      "c",
      "-c",
      source,
      "-I",
      header_dir.string(),
      "-o",
      obj.string()};
  auto compiled = support::RunProcessCaptured(host.cxx, compile_args);
  if (!compiled) {
    return IoError(std::move(compiled.error()));
  }
  if (compiled->exit_code != 0) {
    return diag::Fail(
        diag::DiagCode::kHostBuildFailed,
        std::format(
            "compiling DPI-C source '{}' failed:\n{}", source,
            compiled->stderr_text));
  }
  return obj.string();
}

auto CompileProgram(
    const std::filesystem::path& main_cpp,
    const std::filesystem::path& include_root, const std::filesystem::path& lib,
    const std::filesystem::path& program, const HostBuild& host,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<void> {
  // The generated ABI header sits beside the emitted program source, so that
  // directory is the include path a foreign source resolves it through.
  const std::filesystem::path header_dir = main_cpp.parent_path();
  std::vector<std::string> link_inputs;
  link_inputs.reserve(dpi_inputs.size());
  for (const DpiLinkInput& input : dpi_inputs) {
    auto prepared =
        PrepareDpiLinkInput(host, input, header_dir, program.parent_path());
    if (!prepared) {
      return std::unexpected(std::move(prepared.error()));
    }
    link_inputs.push_back(*std::move(prepared));
  }

  std::vector<std::string> args = {
      std::string(kCxxStandardFlag),
      std::string(OptimizationFlag(host.optimization)), "-I",
      include_root.string()};
  if (auto cached = pch::EnsureCached(
          host.cxx, include_root, host.pch, host.optimization)) {
    args.emplace_back("-include-pch");
    args.push_back(cached->string());
  }
  args.push_back(main_cpp.string());
  for (const std::string& in : link_inputs) {
    args.push_back(in);
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
        std::format(
            "C++ compiler exited with {}:\n{}", result_or->exit_code,
            result_or->stderr_text));
  }
  return {};
}

}  // namespace

auto AssembleProject(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root, const std::filesystem::path& dir,
    SourceFormatting formatting, const HostBuild& host,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<void> {
  if (auto r = EmitAndWriteSources(units, root, dir, formatting); !r) {
    return r;
  }
  if (auto r = WriteDpiSurface(runtime, units, root, dir); !r) {
    return r;
  }
  if (auto r = CopyDpiSources(dpi_inputs, dir); !r) {
    return r;
  }

  const auto script_path = dir / "build.sh";
  if (auto r = WriteFile(
          script_path,
          RenderBuildScript(host.cxx, dpi_inputs, host.optimization));
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
    const std::filesystem::path& dir, const HostBuild& host,
    std::span<const DpiLinkInput> dpi_inputs)
    -> diag::Result<std::filesystem::path> {
  const auto program = dir / kProgramName;
  if (auto r = CompileProgram(
          dir / kMainSource, dir / kRuntimeIncludeDir,
          dir / kRuntimeLibDir / kRuntimeLibFile, program, host, dpi_inputs);
      !r) {
    return std::unexpected(std::move(r.error()));
  }
  return program;
}

auto RunInPlace(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root, const std::filesystem::path& work_dir,
    SourceFormatting formatting, const HostBuild& host,
    std::span<const std::string> child_args,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<int> {
  if (auto r = EmitAndWriteSources(units, root, work_dir, formatting); !r) {
    return std::unexpected(std::move(r.error()));
  }
  if (auto r = WriteDpiSurface(runtime, units, root, work_dir); !r) {
    return std::unexpected(std::move(r.error()));
  }
  const auto program = work_dir / kProgramName;
  if (auto r = CompileProgram(
          work_dir / kMainSource, runtime.include_root, runtime.lib, program,
          host, dpi_inputs);
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
