#include "lyra/compiler/unit_pipeline.hpp"

#include <expected>
#include <utility>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lir/counts.hpp"
#include "lyra/lir/verify.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"
#include "lyra/mir/counts.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::compiler {

void RecordSemanticForm(const mir::CompilationUnit& unit) {
  if (support::RecordsStatistics()) {
    support::RecordCounts(unit.name, "mir", mir::CountsOf(unit));
  }
}

auto LowerUnitToSemantic(
    const hir::CompilationUnit& unit, const diag::SourceManager& source_manager)
    -> diag::Result<mir::CompilationUnit> {
  const profiling::TimeTraceScope span(
      "lower to MIR", [&] { return unit.name; });
  lowering::hir_to_mir::UnitLowerer lowerer(unit, source_manager);
  auto semantic = unit.role == hir::UnitRole::kNamespace
                      ? lowerer.RunNamespace()
                      : lowerer.RunObjectRoot();
  if (semantic) {
    RecordSemanticForm(*semantic);
  }
  return semantic;
}

auto LowerUnitToExecutable(const mir::CompilationUnit& unit)
    -> diag::Result<lir::CompilationUnit> {
  const profiling::TimeTraceScope span(
      "lower to LIR", [&] { return unit.name; });
  auto executable = lowering::mir_to_lir::LowerUnit(unit);
  if (!executable) {
    return std::unexpected(std::move(executable.error()));
  }
  lir::Verify(*executable);
  if (support::RecordsStatistics()) {
    support::RecordCounts(executable->name, "lir", lir::CountsOf(*executable));
  }
  return executable;
}

}  // namespace lyra::compiler
