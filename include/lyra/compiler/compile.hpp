#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

// The name of the synthesized design-root unit. Its constructor elaborates the
// design -- it builds the top-level units as its owned children -- so the host
// constructs this one unit and the runtime walks the tree it builds. A leading
// `$` keeps it distinct from every source unit (an SV identifier cannot begin
// with `$`); a backend maps it to a target-language identifier when emitting.
inline constexpr std::string_view kDesignRootUnitName = "$root";

enum class StopAfter : std::uint8_t { kParse, kHir, kMir, kLir };

// Move-only owning bag of artifacts produced by Compile. Each optional is
// std::nullopt unless the corresponding stage ran and produced a value.
// ParseResult owns the slang Compilation and the SourceManager that
// diagnostics' SourceSpans refer to; callers must keep the artifacts alive
// for the duration of any Diagnostic-resolution work.
struct CompileArtifacts {
  std::optional<frontend::ParseResult> parse;
  std::optional<std::vector<hir::CompilationUnit>> hir_units;
  std::optional<std::vector<mir::CompilationUnit>> mir_units;
  // The synthesized design-root unit, present exactly when `mir_units` is. Its
  // constructor elaborates the design by building the top-level units as its
  // owned children. It is a compiler output distinct from the source units, so
  // the host constructs it directly rather than searching the source set.
  std::optional<mir::CompilationUnit> root_unit;
  // The executable body of each source unit, co-indexed with `mir_units`. Each
  // LIR unit is self-contained -- it owns its own type graph and holds no
  // reference back to the MIR it was lowered from.
  std::optional<std::vector<lir::CompilationUnit>> lir_units;
  // The definition metadata of each compiled unit, co-indexed with `lir_units`:
  // a compiled unit is its executable body plus these immutable source-level
  // facts, held apart because LIR carries no source-language concept. A host
  // builds the runtime definition from the two together.
  std::optional<std::vector<ElaboratedUnitMetadata>> unit_metadata;
  // The design-root unit lowered to its executable body plus metadata, present
  // exactly when `lir_units` is. The execution backend loads it alongside the
  // source units and runs its construct to elaborate the design, the same path
  // the C++ backend takes through the root's constructor.
  std::optional<lir::CompilationUnit> root_lir_unit;
  std::optional<ElaboratedUnitMetadata> root_metadata;
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
