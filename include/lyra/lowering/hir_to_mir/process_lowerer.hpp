#pragma once

#include <string>
#include <utility>
#include <variant>
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
#include "lyra/lowering/hir_to_mir/block_depth.hpp"
#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/method.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

struct AutomaticVarBinding {
  BlockDepth declaration_procedural_depth;
  mir::LocalId var;
};

struct StaticVarBinding {
  mir::MemberId var;
};

// LRM 13.3.1: a static-lifetime body local is realized as a member on
// the callable's owner class, so a HIR procedural-var-ref dispatches to a
// MemberAccess instead of a LocalRef
// (`docs/decisions/variable-lifetime-storage.md`).
using ProceduralVarBinding =
    std::variant<AutomaticVarBinding, StaticVarBinding>;

// Per-process / method lowering registries. Carries facts to the
// surrounding module and class, time resolution, the HIR body, and
// the procedural-var binding table. Traversal state (current block depth,
// active closure capture sink, active with-clause index binding) lives on
// `WalkFrame`, not on the lowerer.
class ProcessLowerer {
 public:
  // Facts: every parameter is set once at construction and never mutated for
  // the lowerer's lifetime. `callable_name` is the synthesized identifier the
  // enclosing scope chose for this callable (LRM processes are anonymous, so
  // the caller passes `"process_N"`; methods pass `src.name`). The
  // `owner_ctor_frame` is the enclosing class's constructor-time
  // frame -- the static-local lowering reads it to place per-instance storage
  // and its init AssignExpr into the owner's constructor_block.
  ProcessLowerer(
      ModuleLowerer& module, const ClassLowerer& lowerer,
      TimeResolution time_resolution, const hir::ProceduralBody& hir_body,
      std::string callable_name, WalkFrame owner_ctor_frame)
      : module_(&module),
        owner_(&lowerer),
        time_resolution_(time_resolution),
        hir_body_(&hir_body),
        callable_name_(std::move(callable_name)),
        owner_ctor_frame_(std::move(owner_ctor_frame)) {
  }

  // Lowers an entire HIR process (initial / final / always / always_ff /
  // always_comb / always_latch) into a `mir::Process`. Constructs the process
  // root scope on the stack and walks `src`'s body into it.
  auto Run(const hir::Process& src) -> diag::Result<mir::Process>;

  // Lowers a HIR subroutine declaration into a `mir::MethodDecl`.
  // Pre-registers the formal params as body locals so call references resolve,
  // then walks the body. Functions with a non-void result close with a
  // trailing `return` of the implicit result variable.
  auto Run(const hir::StructuralSubroutineDecl& src)
      -> diag::Result<mir::MethodDecl>;

  // Central expression dispatcher. One switch over `hir::Expr::data` routing
  // each kind to the per-family handler in `expression/{operators, calls,
  // references, selects, aggregates, assignment, inside}.cpp`. Handlers
  // recurse through this method; sub-expressions reach `frame` through it.
  // An observable-cell leaf is auto-wrapped in an `ObservableMethod{kGet}`
  // call so the result is a value-typed expression.
  auto LowerExpr(const hir::Expr& expr, WalkFrame frame)
      -> diag::Result<mir::Expr>;

  // LHS-context expression dispatcher: same dispatch as `LowerExpr` but
  // without the `ObservableMethod{kGet}` auto-wrap, so an observable-cell
  // leaf flows out as the bare cell expression.
  auto LowerLhsExpr(const hir::Expr& expr, WalkFrame frame)
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

  [[nodiscard]] auto Owner() const -> const ClassLowerer& {
    return *owner_;
  }

  [[nodiscard]] auto Resolution() const -> TimeResolution {
    return time_resolution_;
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

  // The synthesized identifier for the callable being lowered (e.g.
  // `"process_3"`, or a method's user-given name). Consumed by the
  // static-local lowering to mangle the member name on the owner
  // class so that sibling callables sharing a source name (`static int x;` in
  // two processes of the same module) do not collide.
  [[nodiscard]] auto CallableName() const -> std::string_view {
    return callable_name_;
  }

  // The owner class's constructor-time frame. The static-local
  // lowering reads it to place the per-instance storage's init AssignExpr
  // into the owner's constructor block using the owner's own `self` binding,
  // not the body's. Body-walking edits override `current_block` /
  // `self_binding` for the body, while this snapshot stays at the owner's
  // construction-time vantage.
  [[nodiscard]] auto OwnerCtorFrame() const -> const WalkFrame& {
    return owner_ctor_frame_;
  }

 private:
  ModuleLowerer* module_;
  const ClassLowerer* owner_;
  TimeResolution time_resolution_;
  const hir::ProceduralBody* hir_body_;
  std::string callable_name_;
  WalkFrame owner_ctor_frame_;
  std::vector<ProceduralVarBinding> bindings_;
};

}  // namespace lyra::lowering::hir_to_mir
