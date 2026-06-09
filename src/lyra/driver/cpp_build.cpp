#include "lyra/driver/cpp_build.hpp"

#include <array>
#include <cstddef>
#include <filesystem>
#include <format>
#include <fstream>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/support/subprocess.hpp"

namespace lyra::driver {

namespace {

auto IoError(std::string message) {
  return diag::HostError(diag::DiagCode::kHostIoError, std::move(message));
}

auto WriteFile(const std::filesystem::path& path, std::string_view content)
    -> diag::Result<void> {
  std::error_code ec;
  std::filesystem::create_directories(path.parent_path(), ec);
  if (ec) {
    return IoError(
        std::format(
            "failed to create '{}': {}", path.parent_path().string(),
            ec.message()));
  }
  std::ofstream out(path, std::ios::binary);
  out << content;
  out.flush();
  if (!out) {
    return IoError(std::format("failed to write '{}'", path.string()));
  }
  return {};
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
    const auto key = tpl.substr(i, end - i + 1);  // includes both '@'s
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
"$CXX" @STD@ -I @INCLUDE@ $PCH_FLAG @MAIN@ @LIBDIR@/@LIB@ -o @PROG@
)sh";

auto RenderBuildScript() -> std::string {
  const std::array<std::pair<std::string_view, std::string_view>, 8> bindings =
      {{
          {"@INCLUDE@", kRuntimeIncludeDir},
          {"@PRELUDE@", kPreludeHeader},
          {"@CACHE@", kRuntimeCacheDir},
          {"@STD@", kCxxStandardFlag},
          {"@MAIN@", kMainSource},
          {"@LIBDIR@", kRuntimeLibDir},
          {"@LIB@", kRuntimeLibFile},
          {"@PROG@", kProgramName},
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

auto EmitAndWriteSources(
    std::span<const mir::CompilationUnit> units,
    std::span<const backend::cpp::TopInstance> tops,
    const std::filesystem::path& dir, bool format) -> diag::Result<void> {
  auto set_or = backend::cpp::EmitCpp(units, tops);
  if (!set_or) {
    return std::unexpected(std::move(set_or.error()));
  }
  for (const auto& file : set_or->files) {
    if (auto r = WriteFile(dir / file.relpath, file.content); !r) {
      return r;
    }
  }
  if (format) {
    FormatSources(set_or->files, dir);
  }
  return {};
}

auto CompileProgram(
    const std::filesystem::path& main_cpp,
    const std::filesystem::path& include_root, const std::filesystem::path& lib,
    const std::filesystem::path& program, const pch::Options& pch_opts)
    -> diag::Result<void> {
  auto cxx_or = support::ResolveCxxCompiler();
  if (!cxx_or) {
    return IoError(std::move(cxx_or.error()));
  }
  std::vector<std::string> args = {
      std::string(kCxxStandardFlag), "-I", include_root.string()};
  if (auto cached = pch::EnsureCached(*cxx_or, include_root, pch_opts)) {
    args.emplace_back("-include-pch");
    args.push_back(cached->string());
  }
  args.push_back(main_cpp.string());
  args.push_back(lib.string());
  args.emplace_back("-o");
  args.push_back(program.string());
  auto result_or = support::RunProcessCaptured(*cxx_or, args);
  if (!result_or) {
    return IoError(std::move(result_or.error()));
  }
  if (result_or->exit_code != 0) {
    return diag::HostError(
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
    std::span<const backend::cpp::TopInstance> tops,
    const std::filesystem::path& dir, bool format) -> diag::Result<void> {
  if (auto r = EmitAndWriteSources(units, tops, dir, format); !r) {
    return r;
  }

  const auto script_path = dir / "build.sh";
  if (auto r = WriteFile(script_path, RenderBuildScript()); !r) {
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

  if (auto exported = ExportRuntimeTree(runtime, dir); !exported) {
    return IoError(std::move(exported.error()));
  }
  return {};
}

auto BuildProject(
    const std::filesystem::path& dir, const pch::Options& pch_opts)
    -> diag::Result<std::filesystem::path> {
  const auto program = dir / kProgramName;
  if (auto r = CompileProgram(
          dir / kMainSource, dir / kRuntimeIncludeDir,
          dir / kRuntimeLibDir / kRuntimeLibFile, program, pch_opts);
      !r) {
    return std::unexpected(std::move(r.error()));
  }
  return program;
}

auto RunInPlace(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    std::span<const backend::cpp::TopInstance> tops,
    const std::filesystem::path& work_dir, bool format,
    const pch::Options& pch_opts) -> diag::Result<int> {
  if (auto r = EmitAndWriteSources(units, tops, work_dir, format); !r) {
    return std::unexpected(std::move(r.error()));
  }
  const auto program = work_dir / kProgramName;
  if (auto r = CompileProgram(
          work_dir / kMainSource, runtime.include_root, runtime.lib, program,
          pch_opts);
      !r) {
    return std::unexpected(std::move(r.error()));
  }
  auto exit_or = support::RunProcessStreaming(program, {});
  if (!exit_or) {
    return IoError(std::move(exit_or.error()));
  }
  return *exit_or;
}

}  // namespace lyra::driver
