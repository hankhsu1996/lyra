#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

enum class StopAfter : std::uint8_t { kParse, kHir, kMir, kLir };

// Move-only owning bag of artifacts produced by Compile. Each optional is
// std::nullopt unless the corresponding stage ran and produced a value.
// ParseResult owns the slang Compilation and the SourceManager that
// diagnostics' SourceSpans refer to; callers must keep the artifacts alive
// for the duration of any Diagnostic-resolution work.
struct CompileArtifacts {
  std::optional<frontend::ParseResult> parse;
  std::optional<std::vector<hir::ModuleUnit>> hir_units;
  std::optional<std::vector<mir::CompilationUnit>> mir_units;
  // Each LIR unit borrows the same-index MIR unit above for types and metadata,
  // so `lir_units` must not outlive `mir_units`. A vector move keeps the MIR
  // elements at their addresses, so the borrow survives moving these artifacts.
  std::optional<std::vector<lir::CompilationUnit>> lir_units;
  // The definition metadata of each compiled unit, co-indexed with `lir_units`:
  // a compiled unit is its executable body plus these immutable source-level
  // facts, held apart because LIR carries no source-language concept. A host
  // builds the runtime definition from the two together.
  std::optional<std::vector<ElaboratedUnitMetadata>> unit_metadata;
  // A subset of the compiled units: a unit reached only through instantiation
  // is compiled but is not a top.
  std::vector<std::string> top_unit_names;

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
