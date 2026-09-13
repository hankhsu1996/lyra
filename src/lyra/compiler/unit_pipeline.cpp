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
  lowering::hir_to_mir::UnitLowerer lowerer(unit, source_manager);
  auto mir = unit.role == hir::UnitRole::kNamespace ? lowerer.RunNamespace()
                                                    : lowerer.RunObjectRoot();
  if (!mir) {
    return std::unexpected(std::move(mir.error()));
  }
  mir::CompilationUnit lowered = *std::move(mir);
  UnitProgramRecord record = ProgramRecordOf(lowered);
  UnitArtifacts artifacts{
      .mir = std::move(lowered),
      .program_record = std::move(record),
      .lir = std::nullopt,
      .metadata = std::nullopt};

  if (stop_after < StopAfter::kLir) {
    return artifacts;
  }

  auto lir = lowering::mir_to_lir::LowerUnit(artifacts.mir);
  if (!lir) {
    return std::unexpected(std::move(lir.error()));
  }
  artifacts.lir = *std::move(lir);
  lir::Verify(*artifacts.lir);
  artifacts.metadata = BuildUnitMetadata(artifacts.mir);
  return artifacts;
}

}  // namespace lyra::compiler
