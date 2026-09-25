#pragma once

#include <cstddef>
#include <expected>
#include <optional>
#include <type_traits>
#include <utility>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/design_root.hpp"
#include "lyra/compiler/unit_pipeline.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/support/parallel.hpp"

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

// Lowers every declared unit's bodies to HIR and hands each unit on. A unit's
// lowering reads no other unit's, so as many units are in flight at once as
// `width` allows; reaching a unit's HIR reads the front end, which the units
// take in turn, and everything after it runs beside the other units.
//
// What a consumer does with a unit is split along the same line. `produce`
// takes one unit to whatever the consumer wants of it and runs beside the
// other units' lowering, so it reads nothing another unit's `produce` writes.
// `consume` receives what `produce` returned one unit at a time, in the order
// the design lists its units, so what a consumer assembles is the same
// whichever unit happened to finish first. What is resident is the units in
// flight, so the design's size reaches the peak only through what a consumer
// chooses to hold.
//
// Every unit is attempted whatever the others reported, so one run accounts
// for the whole design, and failures are reported in the order the units are
// listed; whether any unit failed is the sink's answer. `produce` answering
// with a diagnostic -- a backend that has no form for the unit -- is reported
// like any other failure inside this stage, and that unit is not consumed.
template <typename Produce, typename Consume>
void LowerToHir(
    ElaboratedDesign& design, diag::DiagnosticSink& sink, std::size_t width,
    Produce produce, Consume consume) {
  using Produced = std::invoke_result_t<Produce, hir::CompilationUnit>;
  support::ProduceInOrder(
      design.units.UnitCount(), width,
      [&](std::size_t i) -> Produced {
        auto unit = design.units.LowerUnit(i);
        if (!unit) {
          return std::unexpected(std::move(unit.error()));
        }
        return produce(*std::move(unit));
      },
      [&](Produced produced) {
        if (produced) {
          consume(*std::move(produced));
        } else {
          sink.Report(std::move(produced.error()));
        }
      });
}

// Models the whole design semantically, then composes the one step no unit
// produces. Nothing comes back when any unit failed: what is short of the
// design is not the design, and holding it to the checks a complete one
// answers would report a stated gap as a bug.
template <typename Produce, typename Consume>
auto LowerToSemantic(
    ElaboratedDesign& design, const diag::SourceManager& sources,
    diag::DiagnosticSink& sink, std::size_t width, Produce produce,
    Consume consume) -> std::optional<SemanticDesign> {
  using Produced = std::invoke_result_t<Produce, SemanticUnit>;
  LowerToHir(
      design, sink, width,
      [&](const hir::CompilationUnit& hir_unit) -> Produced {
        auto unit = LowerUnitToSemantic(hir_unit, sources);
        if (!unit) {
          return std::unexpected(std::move(unit.error()));
        }
        return produce(*std::move(unit));
      },
      consume);
  if (sink.HasErrors()) {
    return std::nullopt;
  }

  auto root =
      SynthesizeDesignRoot(design.tops, design.units.Signatures(), sources);
  if (!root) {
    sink.Report(std::move(root.error()));
    return std::nullopt;
  }
  return SemanticDesign{.root = *std::move(root)};
}

// The same design, taken one layer further: every unit reaches `produce` as
// the body something runs plus the metadata defining it, and the semantic model
// it came from is released on the way.
template <typename Produce, typename Consume>
auto LowerToExecutable(
    ElaboratedDesign& design, const diag::SourceManager& sources,
    diag::DiagnosticSink& sink, std::size_t width, Produce produce,
    Consume consume) -> std::optional<ExecutableDesign> {
  using Produced = std::invoke_result_t<Produce, ExecutableUnit>;
  auto semantic = LowerToSemantic(
      design, sources, sink, width,
      [&](SemanticUnit unit) -> Produced {
        auto executable = LowerUnitToExecutable(unit.mir);
        if (!executable) {
          return std::unexpected(std::move(executable.error()));
        }
        return produce(*std::move(executable));
      },
      consume);
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
