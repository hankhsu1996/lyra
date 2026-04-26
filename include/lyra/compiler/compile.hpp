#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

enum class StopAfter : std::uint8_t { kParse, kHir, kMir };

// Move-only owning bag of artifacts produced by Compile. Each optional is
// std::nullopt unless the corresponding stage ran and produced a value.
// ParseResult owns the slang Compilation and the SourceManager that
// diagnostics' SourceSpans refer to; callers must keep the artifacts alive
// for the duration of any Diagnostic-resolution work.
struct CompileArtifacts {
  std::optional<frontend::ParseResult> parse;
  std::optional<std::vector<hir::ModuleUnit>> hir_units;
  std::optional<mir::CompilationUnit> mir_unit;

  CompileArtifacts() = default;
  CompileArtifacts(const CompileArtifacts&) = delete;
  auto operator=(const CompileArtifacts&) -> CompileArtifacts& = delete;
  CompileArtifacts(CompileArtifacts&&) = default;
  auto operator=(CompileArtifacts&&) -> CompileArtifacts& = default;
  ~CompileArtifacts() = default;
};

// Lyra-owned errors flow through `sink`; slang parse/elaboration errors
// are reported via `slang_ok`. Successful compile requires both
// `!sink.HasErrors()` and `slang_ok`.
struct CompileResult {
  CompileArtifacts artifacts;
  bool slang_ok = true;
};

auto Compile(
    const frontend::CompilationInput& input, diag::DiagnosticSink& sink,
    StopAfter stop_after = StopAfter::kMir) -> CompileResult;

}  // namespace lyra::compiler
