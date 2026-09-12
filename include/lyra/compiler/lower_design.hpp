#pragma once

#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/design_root.hpp"
#include "lyra/compiler/unit_pipeline.hpp"
#include "lyra/compiler/unit_program_record.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"

namespace lyra::compiler {

// What is left once every unit has been lowered and handed on: the record each
// one published, and the synthesized design root. The root's own record is the
// last of them, so a reader of the program's foreign name space sees the
// symbols the root defines alongside the ones the units declare.
struct LoweredDesign {
  std::vector<UnitProgramRecord> records;
  DesignRootArtifacts root;
};

// Drives everything below HIR: each unit's own vertical in turn, then the one
// whole-design step. A unit's artifacts are handed to `consume` and released
// before the next unit is lowered, so what is resident at any moment is one
// unit and the design's size reaches the peak only through what a consumer
// chooses to keep.
//
// Every unit is attempted whatever the ones before it reported, so one run
// accounts for the whole design. Nothing comes back when any of them failed:
// what is short of the design is not the design, and holding it to the checks
// a complete one answers would report a stated gap as a bug.
//
// `consume` answers with a diagnostic when it cannot take a unit -- a backend
// that has no form for it -- which is reported like any other failure inside
// this stage and does not stop the units behind it from being attempted.
template <typename Consume>
auto LowerDesign(
    lowering::ast_to_hir::HirCompilation& hir,
    std::span<const lowering::ast_to_hir::TopLevelUnit> tops,
    StopAfter stop_after, const diag::SourceManager& sources,
    diag::DiagnosticSink& sink, Consume consume)
    -> std::optional<LoweredDesign> {
  std::vector<UnitProgramRecord> records;
  records.reserve(hir.units.size() + 1);
  for (hir::CompilationUnit& slot : hir.units) {
    const hir::CompilationUnit hir_unit = std::move(slot);
    auto unit = LowerUnitPipeline(hir_unit, stop_after, sources);
    if (!unit) {
      sink.Report(std::move(unit.error()));
      continue;
    }
    records.push_back(unit->program_record);
    if (auto taken = consume(std::move(*unit)); !taken) {
      sink.Report(std::move(taken.error()));
    }
  }
  if (sink.HasErrors()) {
    return std::nullopt;
  }

  auto root =
      SynthesizeDesignRoot(records, tops, hir.signatures, stop_after, sources);
  if (!root) {
    sink.Report(std::move(root.error()));
    return std::nullopt;
  }
  records.push_back(ProgramRecordOf(root->mir));
  return LoweredDesign{.records = std::move(records), .root = *std::move(root)};
}

}  // namespace lyra::compiler
