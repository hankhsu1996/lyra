#pragma once

#include <optional>
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
  // The slot's declared MIR type, carried here because the declaring block is
  // not reachable while the enclosing body is still being lowered -- unlike a
  // static var, whose member type is read live from the already-built class
  // arena. A `ref` / `const ref` formal's slot is a `RefType`, so a reference
  // to it lifts to the cell protocol (`kGet` / `kSet` / `kMutate`) instead of
  // reading the unwrapped value.
  mir::TypeId type;
};

struct StaticVarBinding {
  mir::MemberId var;
};

// LRM 13.3.1: a static-lifetime body local is realized as a member on the
// callable's owner class, so a HIR procedural-var-ref dispatches to a
// MemberAccess instead of a LocalRef.
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

  // The expression arena of the body being lowered. The single uniform
  // sub-expression accessor the context-free expression handler templates
  // reach through, identical in shape to `ClassLowerer::HirExprs`.
  [[nodiscard]] auto HirExprs() const
      -> const base::Arena<hir::Expr, hir::ExprId>& {
    return hir_body_->exprs;
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

  // The structural-subroutine tables live on the owning `ClassLowerer`; expose
  // them here too so a templated call handler reaches a user subroutine through
  // the same surface on either pass class (`ClassLowerer` has them directly).
  [[nodiscard]] auto LookupHirSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId id) const
      -> const hir::StructuralSubroutineDecl& {
    return owner_->LookupHirSubroutine(hops, id);
  }
  [[nodiscard]] auto TranslateStructuralSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId id) const
      -> mir::MethodRef {
    return owner_->TranslateStructuralSubroutine(hops, id);
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

  // Assembles the completion-payload value a `return` should carry in the
  // subroutine being lowered: the function's explicit return value (or its
  // implicit result variable when a `return` supplies none) followed by each
  // `output` / `inout` local, normalized to a bare value (one component) or a
  // `TupleExpr` (two or more). Returns nullopt when the payload is empty -- a
  // void function or a task with no outputs -- so the caller emits a plain
  // `return;`. `explicit_value` is the lowered `return expr` operand, absent
  // for a bare `return;`. Reads the pack locals at `frame`'s depth, so a return
  // nested in an inner block resolves the correct hops.
  [[nodiscard]] auto BuildReturnPayload(
      mir::Block& block, const WalkFrame& frame,
      std::optional<mir::ExprId> explicit_value) -> std::optional<mir::ExprId>;

 private:
  ModuleLowerer* module_;
  const ClassLowerer* owner_;
  TimeResolution time_resolution_;
  const hir::ProceduralBody* hir_body_;
  std::string callable_name_;
  WalkFrame owner_ctor_frame_;
  std::vector<ProceduralVarBinding> bindings_;

  // Completion-payload shape of the subroutine being lowered, set by
  // Run(subroutine) before its body walks and read by every return site. All
  // default for a process or an empty-payload callable: no result var, no pack
  // locals, so BuildReturnPayload yields a plain `return;`.
  BlockDepth completion_decl_depth_{};
  std::optional<mir::LocalId> result_var_;
  mir::TypeId result_value_type_{};
  std::vector<mir::LocalId> output_pack_vars_;
  std::vector<mir::TypeId> output_pack_types_;
  mir::TypeId completion_payload_type_{};
};

}  // namespace lyra::lowering::hir_to_mir
