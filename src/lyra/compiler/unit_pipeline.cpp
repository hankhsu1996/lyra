#include "lyra/compiler/unit_pipeline.hpp"

#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lir/counts.hpp"
#include "lyra/lir/verify.hpp"
#include "lyra/lowering/hir_to_mir/type_operation_body.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/counts.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir_transform/transform.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/support/statistics.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::compiler {

namespace {

// How many rounds of building bodies a unit may take. Each round holds what the
// round before it used, and a body uses operations of what its type is made of,
// so the rounds a unit takes follow how deeply its types nest. A generated body
// using a type larger than its own would never end, so that stops the compile
// here, as rustc's collector stops at its recursion limit.
constexpr std::size_t kRecursionLimit = 4096;

}  // namespace

void RecordSemanticForm(const mir::CompilationUnit& unit) {
  if (support::RecordsStatistics()) {
    support::RecordCounts(unit.body_name, "mir", mir::CountsOf(unit));
    support::RecordNestingDepth(
        unit.body_name, "mir", mir::NestingDepthOf(unit));
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

auto LowerUnitToExecutable(
    const mir::CompilationUnit& unit, const mir_transform::PassOptions& passes,
    const TypeOperationCollection& collection)
    -> diag::Result<lir::CompilationUnit> {
  const profiling::TimeTraceScope span(
      "lower to LIR", [&] { return unit.body_name; });
  auto lowered = lowering::mir_to_lir::UnitLowerer::LowerUnit(unit);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  lowering::mir_to_lir::UnitLowerer& lowerer = *lowered;
  const auto transformed_body =
      [&](mir::TypeId type,
          support::ValueOperation answers) -> std::optional<mir::CallableCode> {
    std::optional<mir::CallableCode> built =
        lowering::hir_to_mir::MakeTypeOperationBody(unit, type, answers);
    if (!built.has_value()) {
      return std::nullopt;
    }
    return mir_transform::TransformBody(
        unit, mir::TypeOperationSource{.type = type, .answers = answers},
        *std::move(built), passes);
  };
  // What a body uses comes to light once it is LIR, and a body uses others in
  // turn, so the two alternate until a round finds nothing new.
  const lir::UsedTypeOperations used_since_last = collection(lowerer.Lir());
  for (std::size_t round = 0;; ++round) {
    if (round == kRecursionLimit) {
      throw InternalError(
          std::format(
              "compiler: the bodies built for unit '{}' keep using new ones -- "
              "please report this as a bug",
              unit.body_name));
    }
    lowerer.StateTypeSteps();
    {
      const profiling::TimeTraceScope collecting(
          "collect type operations", [&] { return unit.body_name; });
      for (const lir::TypeOperation& used : used_since_last()) {
        lowerer.MarkOperationUsed(used);
      }
    }
    auto stated = [&] {
      const profiling::TimeTraceScope building(
          "state type operations", [&] { return unit.body_name; });
      return lowerer.StateTypeOperations(transformed_body);
    }();
    if (!stated) {
      return std::unexpected(std::move(stated.error()));
    }
    if (!*stated) {
      break;
    }
  }
  lir::CompilationUnit executable = std::move(lowerer).Finish();
  lir::Verify(executable);
  if (support::RecordsStatistics()) {
    support::RecordCounts(
        executable.body_name, "lir", lir::CountsOf(executable));
  }
  return executable;
}

}  // namespace lyra::compiler
