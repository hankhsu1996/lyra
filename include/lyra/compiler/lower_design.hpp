#pragma once

#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "lyra/base/parallel.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/design_root.hpp"
#include "lyra/compiler/unit_pipeline.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/lowering.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lowering/hir_to_mir/design_instantiations.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir_transform/pass.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/status/status.hpp"

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
  lir::CompilationUnit root;
};

// Lowers every declared unit's lowerings to HIR and hands each on. A
// unit's lowering reads no other unit's, so as many are in flight at once as
// `width` allows; reaching one's HIR reads the front end, which they take in
// the design's order, and everything after it runs beside the others.
//
// Lowerings whose code comes out alike share one body, which
// is all the work after HIR has to run on, and which the first of them in the
// design's order compiles; so which lowering compiles a body,
// and what the body is called, is the same on every run.
//
// What a consumer does with a lowering is split along the same line.
// `produce` takes one to whatever the consumer wants of it and runs beside the
// other units' lowering, so it reads nothing another one's `produce` writes.
// `consume` receives what `produce` returned one at a time, in the order the
// design lists them, so what a consumer assembles is the same whichever
// happened to finish first. What is resident is what is in flight and one copy
// of each body of the unit being lowered, so the design's size
// reaches the peak only through what a consumer chooses to hold.
//
// Every unit is attempted whatever the others reported, so one run accounts
// for the whole design, and failures are reported in the order the units are
// listed; whether any unit failed is the sink's answer. `produce` answering
// with a diagnostic -- a backend that has no form for the unit -- is reported
// like any other failure inside this stage, and that unit is not consumed.
//
// The compiler's own failure while it works on a unit is that unit's failure
// too. A unit reads nothing another unit writes, so one of them breaking an
// invariant says nothing about the rest: it is reported with what was being
// worked on when it broke, and the other units go on.
template <typename Produce, typename Consume>
void LowerToHir(
    ElaboratedDesign& design, diag::DiagnosticSink& sink, std::size_t width,
    Produce produce, Consume consume) {
  using Produced = std::invoke_result_t<Produce, hir::Lowering>;
  const profiling::StageScope stage("lower units");
  status::Pieces(design.units.LoweringCount());
  base::ProduceInOrder(
      design.units.LoweringCount(), width,
      [&](std::size_t i) -> Produced {
        Produced produced = diag::ContainFailure([&]() -> Produced {
          auto lowered = design.units.LowerAt(i);
          if (!lowered) {
            return std::unexpected(std::move(lowered.error()));
          }
          // The lowering is handed on below, so its unit's name is a copy that
          // lasts as long as the work on it.
          const std::string name = lowered->body.name;
          const auto in_unit = diag::FailureContext::InUnit(name);
          const status::Piece piece(lowered->body.source_name);
          return produce(*std::move(lowered));
        });
        if (!produced) {
          status::Errors(1);
        }
        return produced;
      },
      [&](Produced produced) {
        if (produced) {
          consume(*std::move(produced));
        } else {
          sink.Report(std::move(produced.error()));
        }
      });
}

// Models the whole design semantically, each unit rewritten as `passes` asks,
// then composes the one step no unit produces. Nothing comes back when any unit
// failed: what is short of the design is not the design, and holding it to the
// checks a complete one answers would report a stated gap as a bug.
template <typename Produce, typename Consume>
auto LowerToSemantic(
    ElaboratedDesign& design, const diag::SourceManager& sources,
    const mir_transform::PassOptions& passes, diag::DiagnosticSink& sink,
    std::size_t width, Produce produce, Consume consume)
    -> std::optional<SemanticDesign> {
  using Produced = std::invoke_result_t<Produce, mir::CompilationUnit>;
  // What a lowering builds is the one fact of it the design root reads,
  // so it travels with what the consumer made of its body, where it
  // is the first of one, and the two arrive together and in the design's
  // order.
  struct Lowered {
    std::string lowering;
    lowering::hir_to_mir::LoweringMade made;
    std::optional<typename Produced::value_type> produced;
  };
  lowering::hir_to_mir::DesignInstantiations instantiations;
  LowerToHir(
      design, sink, width,
      [&](const hir::Lowering& lowered) -> diag::Result<Lowered> {
        Lowered out{
            .lowering = lowered.name,
            .made = lowering::hir_to_mir::LoweringMade::Of(lowered),
            .produced = std::nullopt};
        if (!lowered.compiles_its_body) return out;
        auto unit = LowerUnitToSemantic(lowered.body, sources, passes);
        if (!unit) {
          return std::unexpected(std::move(unit.error()));
        }
        Produced produced = produce(*std::move(unit));
        if (!produced) {
          return std::unexpected(std::move(produced.error()));
        }
        out.produced = *std::move(produced);
        return out;
      },
      [&](Lowered lowered) {
        instantiations.by_lowering.emplace(
            std::move(lowered.lowering), std::move(lowered.made));
        if (lowered.produced.has_value()) {
          consume(*std::move(lowered.produced));
        }
      });
  if (sink.HasErrors()) {
    return std::nullopt;
  }

  const auto in_unit = diag::FailureContext::InUnit(kDesignRootUnitName);
  auto root = [&] {
    const profiling::StageScope stage("design root");
    return SynthesizeDesignRoot(
        design.units.Tops(), design.units.Signatures(),
        std::move(instantiations), sources, passes);
  }();
  if (!root) {
    sink.Report(std::move(root.error()));
    return std::nullopt;
  }
  return SemanticDesign{.root = *std::move(root)};
}

// The same design, taken one layer further: every unit reaches `produce` as
// the form something runs, and the semantic model it came from is released on
// the way.
template <typename Produce, typename Consume>
auto LowerToExecutable(
    ElaboratedDesign& design, const diag::SourceManager& sources,
    const mir_transform::PassOptions& passes, diag::DiagnosticSink& sink,
    std::size_t width, Produce produce, Consume consume)
    -> std::optional<ExecutableDesign> {
  using Produced = std::invoke_result_t<Produce, lir::CompilationUnit>;
  auto semantic = LowerToSemantic(
      design, sources, passes, sink, width,
      [&](mir::CompilationUnit unit) -> Produced {
        auto executable = LowerUnitToExecutable(unit);
        if (!executable) {
          return std::unexpected(std::move(executable.error()));
        }
        return produce(*std::move(executable));
      },
      consume);
  if (!semantic) {
    return std::nullopt;
  }

  const auto in_unit = diag::FailureContext::InUnit(kDesignRootUnitName);
  auto root = [&] {
    const profiling::StageScope stage("lower root to LIR");
    return LowerUnitToExecutable(semantic->root);
  }();
  if (!root) {
    sink.Report(std::move(root.error()));
    return std::nullopt;
  }
  return ExecutableDesign{.root = *std::move(root)};
}

}  // namespace lyra::compiler
