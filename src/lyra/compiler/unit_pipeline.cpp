#include "lyra/compiler/unit_pipeline.hpp"

#include <expected>
#include <utility>

#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lir/verify.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"

namespace lyra::compiler {

auto LowerUnitPipeline(
    const hir::CompilationUnit& unit, StopAfter stop_after,
    const diag::SourceManager& source_manager) -> diag::Result<UnitArtifacts> {
  UnitArtifacts artifacts;
  if (stop_after < StopAfter::kMir) {
    return artifacts;
  }

  lowering::hir_to_mir::UnitLowerer lowerer(unit, source_manager);
  auto mir = unit.kind == hir::UnitKind::kPackage ? lowerer.RunPackage()
                                                  : lowerer.RunModule();
  if (!mir) {
    return std::unexpected(std::move(mir.error()));
  }
  artifacts.mir = *std::move(mir);

  // A package is a namespace unit (LRM 26): it has no executable body, so it
  // never joins the LIR path.
  if (unit.kind == hir::UnitKind::kPackage || stop_after < StopAfter::kLir) {
    return artifacts;
  }

  auto lir = lowering::mir_to_lir::LowerUnit(*artifacts.mir);
  if (!lir) {
    return std::unexpected(std::move(lir.error()));
  }
  artifacts.lir = *std::move(lir);
  lir::Verify(*artifacts.lir);
  artifacts.metadata = BuildUnitMetadata(*artifacts.mir);
  return artifacts;
}

}  // namespace lyra::compiler
