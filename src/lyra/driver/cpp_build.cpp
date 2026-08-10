#include "lyra/driver/cpp_build.hpp"

#include <array>
#include <cstddef>
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
# Build this self-contained Lyra C++ project. Override the compiler with $CXX.
# A precompiled header is built on first run and reused on subsequent rebuilds
# to amortize parsing of the runtime headers (clang only).
# Disable with LYRA_NO_PCH=1.
set -e
CXX="${CXX:-clang++}"
USE_PCH=0
if [ -z "$LYRA_NO_PCH" ] || [ "$LYRA_NO_PCH" = "0" ]; then
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
  PCH="@CACHE@/prelude-${FP}.pch"
  if [ ! -f "$PCH" ]; then
    mkdir -p "$(dirname "$PCH")"
    "$CXX" @STD@ -I @INCLUDE@ -xc++-header "$PRELUDE" -o "$PCH"
  fi
  PCH_FLAG="-include-pch $PCH"
fi
@DPICOMPILE@"$CXX" @STD@ -I @INCLUDE@ $PCH_FLAG @MAIN@@DPIOBJS@ @LIBDIR@/@LIB@ -o @PROG@
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

auto RenderDpiRecipe(std::span<const DpiLinkInput> inputs) -> DpiRecipe {
  DpiRecipe recipe;
  for (const DpiLinkInput& input : inputs) {
    const std::string relative = DpiSourceRelPath(input);
    if (!input.compile_as_c) {
      recipe.link_inputs += std::format(" {}", relative);
      continue;
    }
    const std::string object = relative + ".o";
    recipe.compile_steps +=
        std::format("\"$CXX\" -x c -c {} -I . -o {}\n", relative, object);
    recipe.link_inputs += std::format(" {}", object);
  }
  return recipe;
}

auto RenderBuildScript(std::span<const DpiLinkInput> dpi_inputs)
    -> std::string {
  const DpiRecipe dpi = RenderDpiRecipe(dpi_inputs);
  const std::array<std::pair<std::string_view, std::string_view>, 10> bindings =
      {{
          {"@INCLUDE@", kRuntimeIncludeDir},
          {"@PRELUDE@", support::kRuntimePreludeHeader},
          {"@CACHE@", kRuntimeCacheDir},
          {"@STD@", kCxxStandardFlag},
          {"@MAIN@", kMainSource},
          {"@LIBDIR@", kRuntimeLibDir},
          {"@LIB@", kRuntimeLibFile},
          {"@PROG@", kProgramName},
          {"@DPICOMPILE@", dpi.compile_steps},
          {"@DPIOBJS@", dpi.link_inputs},
      }};
  return SubstituteTokens(kBuildScriptTemplate, bindings);
}

// Best-effort: reformat the emitted C++ files in place with clang-format if it
// is on PATH. A missing formatter or a non-zero exit is ignored -- the emitted
// code is valid C++ either way, so formatting never gates emission.
void FormatSources(
    std::span<const backend::cpp::CppArtifact> files,
    const std::filesystem::path& dir) {
  auto clang_format = support::FindOnPath("clang-format");
  if (!clang_format) {
    return;
  }
  std::vector<std::string> args = {"-i", "-style=Google"};
  for (const auto& file : files) {
    args.push_back((dir / file.relpath).string());
  }
  (void)support::RunProcessCaptured(*clang_format, args);
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
    bool format) -> diag::Result<void> {
  auto set = backend::cpp::EmitCpp(units, root);
  for (const auto& file : set.files) {
    if (auto r = WriteFile(dir / file.relpath, file.content); !r) {
      return r;
    }
  }
  if (format) {
    FormatSources(set.files, dir);
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
    const std::filesystem::path& cxx, const DpiLinkInput& input,
    const std::filesystem::path& header_dir,
    const std::filesystem::path& work_dir) -> diag::Result<std::string> {
  const std::string source = input.source.string();
  if (!input.compile_as_c) {
    return source;
  }
  const std::filesystem::path obj =
      work_dir / (input.source.filename().string() + ".o");
  const std::vector<std::string> compile_args = {
      "-x", "c", "-c", source, "-I", header_dir.string(), "-o", obj.string()};
  auto compiled = support::RunProcessCaptured(cxx, compile_args);
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
    const std::filesystem::path& program, const pch::Options& pch_opts,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<void> {
  auto cxx_or = support::ResolveCxxCompiler();
  if (!cxx_or) {
    return IoError(std::move(cxx_or.error()));
  }
  // The generated ABI header sits beside the emitted program source, so that
  // directory is the include path a foreign source resolves it through.
  const std::filesystem::path header_dir = main_cpp.parent_path();
  std::vector<std::string> link_inputs;
  link_inputs.reserve(dpi_inputs.size());
  for (const DpiLinkInput& input : dpi_inputs) {
    auto prepared =
        PrepareDpiLinkInput(*cxx_or, input, header_dir, program.parent_path());
    if (!prepared) {
      return std::unexpected(std::move(prepared.error()));
    }
    link_inputs.push_back(*std::move(prepared));
  }

  std::vector<std::string> args = {
      std::string(kCxxStandardFlag), "-I", include_root.string()};
  if (auto cached = pch::EnsureCached(*cxx_or, include_root, pch_opts)) {
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
  auto result_or = support::RunProcessCaptured(*cxx_or, args);
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
    bool format, std::span<const DpiLinkInput> dpi_inputs)
    -> diag::Result<void> {
  if (auto r = EmitAndWriteSources(units, root, dir, format); !r) {
    return r;
  }
  if (auto r = WriteDpiSurface(runtime, units, root, dir); !r) {
    return r;
  }
  if (auto r = CopyDpiSources(dpi_inputs, dir); !r) {
    return r;
  }

  const auto script_path = dir / "build.sh";
  if (auto r = WriteFile(script_path, RenderBuildScript(dpi_inputs)); !r) {
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
    const std::filesystem::path& dir, const pch::Options& pch_opts,
    std::span<const DpiLinkInput> dpi_inputs)
    -> diag::Result<std::filesystem::path> {
  const auto program = dir / kProgramName;
  if (auto r = CompileProgram(
          dir / kMainSource, dir / kRuntimeIncludeDir,
          dir / kRuntimeLibDir / kRuntimeLibFile, program, pch_opts,
          dpi_inputs);
      !r) {
    return std::unexpected(std::move(r.error()));
  }
  return program;
}

auto RunInPlace(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root, const std::filesystem::path& work_dir,
    bool format, const pch::Options& pch_opts,
    std::span<const std::string> child_args,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<int> {
  if (auto r = EmitAndWriteSources(units, root, work_dir, format); !r) {
    return std::unexpected(std::move(r.error()));
  }
  if (auto r = WriteDpiSurface(runtime, units, root, work_dir); !r) {
    return std::unexpected(std::move(r.error()));
  }
  const auto program = work_dir / kProgramName;
  if (auto r = CompileProgram(
          work_dir / kMainSource, runtime.include_root, runtime.lib, program,
          pch_opts, dpi_inputs);
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
