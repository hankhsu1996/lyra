#include "lyra/compiler/unit_pipeline.hpp"

#include <expected>
#include <utility>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lir/verify.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"

namespace lyra::compiler {

auto LowerUnitToSemantic(
    const hir::CompilationUnit& unit, const diag::SourceManager& source_manager)
    -> diag::Result<mir::CompilationUnit> {
  lowering::hir_to_mir::UnitLowerer lowerer(unit, source_manager);
  return unit.role == hir::UnitRole::kNamespace ? lowerer.RunNamespace()
                                                : lowerer.RunObjectRoot();
}

auto LowerUnitToExecutable(const mir::CompilationUnit& unit)
    -> diag::Result<lir::CompilationUnit> {
  auto executable = lowering::mir_to_lir::LowerUnit(unit);
  if (!executable) {
    return std::unexpected(std::move(executable.error()));
  }
  lir::Verify(*executable);
  return executable;
}

}  // namespace lyra::compiler
