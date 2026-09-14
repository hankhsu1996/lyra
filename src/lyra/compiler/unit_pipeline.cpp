#include "lyra/compiler/unit_pipeline.hpp"

#include <expected>
#include <utility>

#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/compiler/unit_program_record.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lir/verify.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"

namespace lyra::compiler {

auto LowerUnitToSemantic(
    const hir::CompilationUnit& unit, const diag::SourceManager& source_manager)
    -> diag::Result<SemanticUnit> {
  lowering::hir_to_mir::UnitLowerer lowerer(unit, source_manager);
  auto mir = unit.role == hir::UnitRole::kNamespace ? lowerer.RunNamespace()
                                                    : lowerer.RunObjectRoot();
  if (!mir) {
    return std::unexpected(std::move(mir.error()));
  }
  mir::CompilationUnit lowered = *std::move(mir);
  UnitProgramRecord record = ProgramRecordOf(lowered);
  return SemanticUnit{
      .mir = std::move(lowered), .program_record = std::move(record)};
}

auto LowerUnitToExecutable(const mir::CompilationUnit& unit)
    -> diag::Result<ExecutableUnit> {
  auto body = lowering::mir_to_lir::LowerUnit(unit);
  if (!body) {
    return std::unexpected(std::move(body.error()));
  }
  lir::Verify(*body);
  return ExecutableUnit{
      .body = *std::move(body), .definition = BuildUnitMetadata(unit)};
}

}  // namespace lyra::compiler
