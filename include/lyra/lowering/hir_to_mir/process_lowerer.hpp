#pragma once

#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/procedural_depth.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

struct ProceduralVarBinding {
  ProceduralDepth declaration_procedural_depth;
  mir::ProceduralVarId var;
};

// Per-process / subroutine lowering registries. Carries facts to the
// surrounding module and structural scope, time resolution, the HIR body,
// the procedural-var binding table, and the static-locals collection.
// Traversal state (current procedural depth, active closure capture sink,
// active with-clause index binding) and the static-frame scope pointer live
// on `WalkFrame`, not on the lowerer.
class ProcessLowerer {
 public:
  ProcessLowerer(
      ModuleLowerer& module, const StructuralScopeLowerer& scope,
      TimeResolution time_resolution, const hir::ProceduralBody& hir_body)
      : module_(&module),
        scope_(&scope),
        time_resolution_(time_resolution),
        hir_body_(&hir_body) {
  }

  // Lowers an entire HIR process (initial / final / always / always_ff /
  // always_comb / always_latch) into a `mir::Process`. Constructs the process
  // root scope on the stack and walks `src`'s body into it.
  auto Run(WalkFrame parent_frame, const hir::Process& src)
      -> diag::Result<mir::Process>;

  // Lowers a HIR subroutine declaration into a `mir::StructuralSubroutineDecl`.
  // Pre-registers the formal params as body locals so call references resolve,
  // then walks the body. Functions with a non-void result close with a
  // trailing `return` of the implicit result variable.
  auto Run(WalkFrame parent_frame, const hir::StructuralSubroutineDecl& src)
      -> diag::Result<mir::StructuralSubroutineDecl>;

  // Central expression dispatcher. One switch over `hir::Expr::data` routing
  // each kind to the per-family handler in `expression/{operators, calls,
  // references, selects, aggregates, assignment, inside}.cpp`. Handlers
  // recurse through this method; sub-expressions reach `frame` through it.
  auto LowerExpr(const hir::Expr& expr, WalkFrame frame)
      -> diag::Result<mir::Expr>;

  // Central statement dispatcher. One switch over `hir::Stmt::data` routing
  // each kind to the per-family handler in `statement/{blocks, branches,
  // loops, timing, fork_join, assignment, flow}.cpp`.
  auto LowerStmt(const hir::Stmt& stmt, WalkFrame frame)
      -> diag::Result<mir::Stmt>;

  [[nodiscard]] auto HirBody() const -> const hir::ProceduralBody& {
    return *hir_body_;
  }

  [[nodiscard]] auto Module() -> ModuleLowerer& {
    return *module_;
  }
  [[nodiscard]] auto Module() const -> const ModuleLowerer& {
    return *module_;
  }

  [[nodiscard]] auto Scope() const -> const StructuralScopeLowerer& {
    return *scope_;
  }

  [[nodiscard]] auto Resolution() const -> TimeResolution {
    return time_resolution_;
  }

  void AddStaticLocal(mir::StaticLocal local) {
    static_locals_.push_back(local);
  }
  auto TakeStaticLocals() -> std::vector<mir::StaticLocal> {
    return std::move(static_locals_);
  }

  void MapProceduralVar(
      hir::ProceduralVarId hir_id, ProceduralVarBinding binding) {
    if (hir_id.value != bindings_.size()) {
      throw InternalError(
          "ProcessLowerer::MapProceduralVar: HIR procedural vars must "
          "be mapped in HIR id order");
    }
    bindings_.push_back(binding);
  }

  [[nodiscard]] auto LookupProceduralVar(hir::ProceduralVarId hir_id) const
      -> const ProceduralVarBinding& {
    if (hir_id.value >= bindings_.size()) {
      throw InternalError(
          "ProcessLowerer::LookupProceduralVar: unmapped HIR "
          "procedural var");
    }
    return bindings_[hir_id.value];
  }

  // Computes a `mir::ProceduralVarRef` to `hir_id` from the current walker
  // position. `frame.procedural_depth` is the reading depth; the declaration
  // depth comes from the registry. A forward reference into a child scope is
  // a lowering bug.
  [[nodiscard]] auto TranslateProceduralVar(
      const WalkFrame& frame, hir::ProceduralVarId hir_id) const
      -> mir::ProceduralVarRef {
    const auto& binding = LookupProceduralVar(hir_id);
    if (binding.declaration_procedural_depth > frame.procedural_depth) {
      throw InternalError(
          "ProcessLowerer::TranslateProceduralVar: declaration depth "
          "exceeds current depth (forward reference into a child scope)");
    }
    return mir::ProceduralVarRef{
        .hops = frame.procedural_depth - binding.declaration_procedural_depth,
        .var = binding.var,
    };
  }

 private:
  ModuleLowerer* module_;
  const StructuralScopeLowerer* scope_;
  TimeResolution time_resolution_;
  const hir::ProceduralBody* hir_body_;
  std::vector<ProceduralVarBinding> bindings_;
  std::vector<mir::StaticLocal> static_locals_;
};

}  // namespace lyra::lowering::hir_to_mir
