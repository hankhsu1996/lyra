#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include <slang/driver/Driver.h>

#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace lyra::compiler {

// The name of the synthesized design-root unit. Its constructor elaborates the
// design -- it builds the top-level units as its owned children -- so the host
// constructs this one unit and the runtime walks the tree it builds. A leading
// `$` keeps it distinct from every source unit (an SV identifier cannot begin
// with `$`); a backend maps it to a target-language identifier when emitting.
inline constexpr std::string_view kDesignRootUnitName = "$root";

// The last stage a compile runs, in pipeline order. A stage asks whether the
// request reaches past it and so compares position rather than naming a
// member, which makes this an ordered scale and not a dispatch set.
enum class StopAfter : std::uint8_t { kParse, kHir, kMir, kLir };

// What reading the elaborated AST produces: which units the design begins at,
// and the HIR of every unit. Neither member carries a front-end node, so the
// AST is read by one call and has to outlive nothing but it.
struct ElaboratedDesign {
  std::vector<lowering::ast_to_hir::TopLevelUnit> tops;
  lowering::ast_to_hir::HirCompilation hir;
};

// Move-only owning bag of what the front end produced for its caller to read.
// A stage's product is present exactly when the request reads it, which is not
// the same as the stage having run: the elaborated AST is released as soon as
// lowering to HIR has read it, because holding a whole design's worth of it to
// the end of the run is what a peak is made of. So absence here says "this
// request does not read that", never "that stage did not run".
//
// Nothing below HIR is here. A unit's lowered form is handed to whoever asked
// for it and released before the next unit is lowered, so it belongs to that
// reader for as long as it exists and to no bag at all.
//
// The SourceManager a diagnostic's spans resolve through is not a stage
// product and lives as long as the artifacts do, which callers must keep alive
// for the duration of any Diagnostic-resolution work.
struct CompileArtifacts {
  std::optional<frontend::ParseResult> parse;
  std::optional<ElaboratedDesign> design;

  // Reading a stage's product asserts that the stage ran. Which optionals are
  // filled follows from how far down the pipeline the caller asked to go, and
  // nothing in this type carries that choice: asking for HIR after stopping at
  // elaboration is a driver bug, and a bare dereference makes it undefined
  // behaviour instead of a report. Each accessor names the stage it wanted.
  //
  // Const, yet the compilation it hands back is not: what these artifacts own
  // is the pointer, and slang takes its own compilation mutably to serialize
  // or to look up the design root.
  [[nodiscard]] auto Elaboration() const -> slang::ast::Compilation&;
  [[nodiscard]] auto Design() const -> const ElaboratedDesign&;
  // The design's HIR, to be drained: the stage below releases each unit as it
  // lowers it, which is what keeps one unit resident instead of the design.
  [[nodiscard]] auto DesignToLower() -> ElaboratedDesign&;

  CompileArtifacts() = default;
  CompileArtifacts(const CompileArtifacts&) = delete;
  auto operator=(const CompileArtifacts&) -> CompileArtifacts& = delete;
  CompileArtifacts(CompileArtifacts&&) = default;
  auto operator=(CompileArtifacts&&) -> CompileArtifacts& = default;
  ~CompileArtifacts() = default;
};

// Choices about how to lower that the design does not make for itself.
struct LoweringPolicy {
  support::AssertionPolicy assertions = support::AssertionPolicy::kCheck;
};

// Lyra-owned errors flow through `sink`; slang parse/elaboration errors are
// reported via `slang_ok` and rendered into `slang_diagnostics`. Successful
// compile requires both `!sink.HasErrors()` and `slang_ok`.
struct CompileResult {
  CompileArtifacts artifacts;
  bool slang_ok = true;
  std::string slang_diagnostics;
};

// Runs the front end and lowers the compilation to HIR. Everything below HIR
// is per unit and belongs to whoever consumes it, so it is driven separately:
// a caller that wants MIR or an emitted artifact runs `LowerDesign` over what
// comes back here and takes each unit as it is produced.
//
// The driver arrives configured -- its options parsed, its sources named --
// and owns the text every resulting span points into, so it must outlive the
// returned artifacts.
auto Compile(
    slang::driver::Driver& driver, LoweringPolicy policy,
    diag::DiagnosticSink& sink, StopAfter stop_after) -> CompileResult;

}  // namespace lyra::compiler
