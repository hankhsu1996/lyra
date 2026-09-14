#pragma once

#include <expected>
#include <optional>
#include <utility>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/design_root.hpp"
#include "lyra/compiler/unit_pipeline.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

// What is left of a design once every unit has been modelled and handed on: the
// synthesized design root, which is the one artifact no unit produced. Nothing
// of the units survives here -- whatever a consumer wanted of one it took while
// the unit was in its hands.
struct SemanticDesign {
  mir::CompilationUnit root;
};

// The same, for a design taken all the way to the form something runs.
struct ExecutableDesign {
  ExecutableUnit root;
};

// Models the whole design semantically, then composes the one step no unit
// produces. Each unit is handed to `consume` and released before the next is
// lowered, so what is resident at any moment is one unit and the design's size
// reaches the peak only through what a consumer chooses to keep.
//
// Every unit is attempted whatever the ones before it reported, so one run
// accounts for the whole design. Nothing comes back when any of them failed:
// what is short of the design is not the design, and holding it to the checks a
// complete one answers would report a stated gap as a bug.
//
// `consume` answers with a diagnostic when it cannot take a unit -- a backend
// that has no form for it -- which is reported like any other failure inside
// this stage and does not stop the units behind it from being attempted.
template <typename Consume>
auto LowerToSemantic(
    ElaboratedDesign& design, const diag::SourceManager& sources,
    diag::DiagnosticSink& sink, Consume consume)
    -> std::optional<SemanticDesign> {
  for (hir::CompilationUnit& slot : design.hir.units) {
    const hir::CompilationUnit hir_unit = std::move(slot);
    auto unit = LowerUnitToSemantic(hir_unit, sources);
    if (!unit) {
      sink.Report(std::move(unit.error()));
      continue;
    }
    if (auto taken = consume(*std::move(unit)); !taken) {
      sink.Report(std::move(taken.error()));
    }
  }
  if (sink.HasErrors()) {
    return std::nullopt;
  }

  auto root = SynthesizeDesignRoot(design.tops, design.hir.signatures, sources);
  if (!root) {
    sink.Report(std::move(root.error()));
    return std::nullopt;
  }
  return SemanticDesign{.root = *std::move(root)};
}

// The same design, taken one layer further: every unit reaches `consume` as the
// body something runs plus the metadata defining it, and the semantic model it
// came from is released on the way.
template <typename Consume>
auto LowerToExecutable(
    ElaboratedDesign& design, const diag::SourceManager& sources,
    diag::DiagnosticSink& sink, Consume consume)
    -> std::optional<ExecutableDesign> {
  auto semantic = LowerToSemantic(
      design, sources, sink, [&](SemanticUnit unit) -> diag::Result<void> {
        auto executable = LowerUnitToExecutable(unit.mir);
        if (!executable) {
          return std::unexpected(std::move(executable.error()));
        }
        return consume(*std::move(executable));
      });
  if (!semantic) {
    return std::nullopt;
  }

  auto root = LowerUnitToExecutable(semantic->root);
  if (!root) {
    sink.Report(std::move(root.error()));
    return std::nullopt;
  }
  return ExecutableDesign{.root = *std::move(root)};
}

}  // namespace lyra::compiler
