#include "lyra/compiler/unit_pipeline.hpp"

#include <expected>
#include <utility>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lir/counts.hpp"
#include "lyra/lir/verify.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/counts.hpp"
#include "lyra/mir_transform/transform.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::compiler {

void RecordSemanticForm(const mir::CompilationUnit& unit) {
  if (support::RecordsStatistics()) {
    support::RecordCounts(unit.body_name, "mir", mir::CountsOf(unit));
  }
}

auto LowerUnitToSemantic(
    const hir::CompilationUnit& unit, const diag::SourceManager& source_manager,
    const mir_transform::PassOptions& passes)
    -> diag::Result<mir::CompilationUnit> {
  const profiling::TimeTraceScope span(
      "lower to MIR", [&] { return unit.body_name; });
  lowering::hir_to_mir::UnitLowerer lowerer(unit, source_manager);
  auto semantic = hir::RootsAnObject(unit.role) ? lowerer.RunObjectRoot()
                                                : lowerer.RunNamespace();
  if (!semantic) {
    return semantic;
  }
  mir::CompilationUnit transformed =
      mir_transform::Transform(*std::move(semantic), passes);
  RecordSemanticForm(transformed);
  return transformed;
}

auto LowerUnitToExecutable(const mir::CompilationUnit& unit)
    -> diag::Result<lir::CompilationUnit> {
  const profiling::TimeTraceScope span(
      "lower to LIR", [&] { return unit.body_name; });
  auto executable = lowering::mir_to_lir::LowerUnit(unit);
  if (!executable) {
    return std::unexpected(std::move(executable.error()));
  }
  lir::Verify(*executable);
  if (support::RecordsStatistics()) {
    support::RecordCounts(
        executable->body_name, "lir", lir::CountsOf(*executable));
  }
  return executable;
}

}  // namespace lyra::compiler
