#pragma once

#include <map>
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
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_storage_plan.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/method.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

struct AutomaticVarBinding {
  // The slot's declared MIR type. A `ref` / `const ref` formal's slot is a
  // `RefType`, so a reference to it lifts to the cell protocol (`kGet` / `kSet`
  // / `kMutate`) instead of reading the unwrapped value. The binding's
  // cross-body identity is its HIR procedural-var id, materialized through the
  // callable's binding context, not stored here.
  mir::TypeId type;
};

// LRM 6.21: an automatic local a detached fork branch borrows and can
// outlive is lifted into a shared activation object. Its reads / writes
// reach a field of that object through a shared-pointer handle; a branch
// captures the handle by value to keep the activation alive across the
// declaring frame's return.
struct PromotedVarBinding {
  BindingOriginId handle_origin;
  mir::TypeId handle_type;
  mir::FieldId field;
};

// Body-local environment binding for one HIR procedural var: an in-frame
// local for an automatic, or a shared-activation handle for a
// lifetime-extended automatic (LRM 6.21). Static-lifetime locals (LRM
// 13.3.1) are NOT body-local; their placement is the per-callable storage
// plan's authority, not this binding registry.
using ProceduralVarBinding =
    std::variant<AutomaticVarBinding, PromotedVarBinding>;

// Per-process / method lowering registries. Carries facts to the
// surrounding module and class, time resolution, the HIR body, and
// the procedural-var binding table. Per-recursion traversal state -- the
// current block and class, the binding-resolution context, and whether the
// body is a coroutine -- lives on `WalkFrame`, not on the lowerer.
class ProcessLowerer {
 public:
  // Facts: every parameter is set once at construction and never mutated for
  // the lowerer's lifetime. `enclosing_scope_lowerer` is the lowering pass
  // for the structural scope this body sits inside; its registries resolve
  // every reference to an enclosing-scope declaration. It is null for a
  // class method body, which sits inside no structural scope. `callable_name`
  // is the synthesized identifier the enclosing scope chose for this
  // callable (LRM processes are anonymous, so the caller passes
  // `"process_N"`; methods pass the user-given name). `owner_ctor_frame` is
  // the enclosing class's constructor-time frame -- the base frame each
  // body lowering extends with its own block / bindings, carrying the
  // owner-class context (self pointer type, scope chain) into body lowering.
  // `visibility` is the access the produced method declares (public for a
  // class instance method, internal for a scope's processes and
  // subroutines). `storage_plan` is the planner's per-callable storage plan
  // -- static placements and the shared scope materialization table that
  // chain queries route through.
  ProcessLowerer(
      ModuleLowerer& module,
      const StructuralScopeLowerer* enclosing_scope_lowerer,
      TimeResolution time_resolution, const hir::ProceduralBody& hir_body,
      std::string callable_name, mir::MethodVisibility visibility,
      WalkFrame owner_ctor_frame, const CallableStoragePlan& storage_plan)
      : module_(&module),
        enclosing_scope_lowerer_(enclosing_scope_lowerer),
        time_resolution_(time_resolution),
        hir_body_(&hir_body),
        callable_name_(std::move(callable_name)),
        visibility_(visibility),
        owner_ctor_frame_(std::move(owner_ctor_frame)),
        storage_plan_(&storage_plan) {
  }

  // Lowers an entire HIR process (initial / final / always / always_ff /
  // always_comb / always_latch) into its `mir::MethodDecl`. Any static
  // initializers the body defers to the Initialize phase accumulate on the
  // lowerer; the caller drains them through `TakePendingStaticInitializers`
  // after `Run` returns and integrates them into the owning class's
  // Initialize block.
  auto Run(const hir::Process& src) -> diag::Result<mir::MethodDecl>;

  // Lowers a HIR subroutine declaration into a `mir::MethodDecl`.
  // Pre-registers the formal params as body locals so call references resolve,
  // then walks the body. Functions with a non-void result close with a
  // trailing `return` of the implicit result variable. Deferred static
  // initializers are drained the same way as for a process body.
  auto Run(const hir::SubroutineDecl& src) -> diag::Result<mir::MethodDecl>;

  // Central expression dispatcher. One switch over `hir::Expr::data` routing
  // each kind to its per-family handler; handlers recurse through this
  // method, so sub-expressions reach `frame` through it. An observable-cell
  // leaf is auto-wrapped in an `ObservableMethod{kGet}` call so the result
  // is a value-typed expression.
  auto LowerExpr(const hir::Expr& expr, WalkFrame frame)
      -> diag::Result<mir::Expr>;

  // LHS-context expression dispatcher: same dispatch as `LowerExpr` but
  // without the `ObservableMethod{kGet}` auto-wrap, so an observable-cell
  // leaf flows out as the bare cell expression.
  auto LowerLhsExpr(const hir::Expr& expr, WalkFrame frame)
      -> diag::Result<mir::Expr>;

  // Central statement dispatcher. One switch over `hir::Stmt::data` routing
  // each kind to its per-family handler.
  auto LowerStmt(const hir::Stmt& stmt, WalkFrame frame)
      -> diag::Result<mir::Stmt>;

  [[nodiscard]] auto HirBody() const -> const hir::ProceduralBody& {
    return *hir_body_;
  }

  // The expression arena of the body being lowered. The uniform sub-expression
  // accessor the context-free expression handler templates reach through; both
  // lowering pass classes expose it with the same shape so those templates bind
  // to either.
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

  // The lowering pass for the structural scope this body sits inside; its
  // registries resolve every reference to an enclosing-scope declaration (a
  // structural variable, a generate loop variable, a cross-unit reference, a
  // peer subroutine). A class method body resolves no such reference and sits
  // inside no structural scope; reaching this from one is a compiler bug.
  [[nodiscard]] auto EnclosingScopeLowerer() const
      -> const StructuralScopeLowerer& {
    if (enclosing_scope_lowerer_ == nullptr) {
      throw InternalError(
          "ProcessLowerer::EnclosingScopeLowerer: a class method body sits "
          "inside no structural scope");
    }
    return *enclosing_scope_lowerer_;
  }

  // The structural-subroutine tables live on the owning structural-scope pass;
  // this forwards to them so a templated call handler reaches a user subroutine
  // through the same surface on either pass class.
  [[nodiscard]] auto LookupHirSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId id) const
      -> const hir::SubroutineDecl& {
    return EnclosingScopeLowerer().LookupHirSubroutine(hops, id);
  }
  [[nodiscard]] auto TranslateStructuralSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId id) const
      -> mir::Direct {
    return EnclosingScopeLowerer().TranslateStructuralSubroutine(hops, id);
  }

  [[nodiscard]] auto Resolution() const -> TimeResolution {
    return time_resolution_;
  }

  void MapProceduralVar(
      hir::ProceduralVarId hir_id, ProceduralVarBinding binding) {
    if (hir_id.value >= bindings_.size()) {
      bindings_.resize(hir_id.value + 1);
    }
    if (bindings_[hir_id.value].has_value()) {
      throw InternalError(
          "ProcessLowerer::MapProceduralVar: HIR procedural var already "
          "registered in the body-local environment");
    }
    bindings_[hir_id.value] = binding;
  }

  // Returns the body-local environment binding for an automatic / promoted
  // var, or nullptr when the var has no body-local binding (it resolves
  // through `LookupStaticPlacement` against the storage plan instead).
  // Reference-resolution sites dispatch on this -- presence in the body
  // env is the authoritative signal, not the HIR lifetime, because
  // subroutine params / output / result_var get registered here regardless
  // of the slang-reported lifetime.
  [[nodiscard]] auto LookupProceduralVar(hir::ProceduralVarId hir_id) const
      -> const ProceduralVarBinding* {
    if (hir_id.value >= bindings_.size() ||
        !bindings_[hir_id.value].has_value()) {
      return nullptr;
    }
    return &*bindings_[hir_id.value];
  }

  // An activation scope is opened at block entry -- the scope struct and its
  // handle are built then -- but a promoted var's binding must register in HIR
  // id order at its declaration like any other. The block-entry pass records
  // the slot here; the declaration consumes it (`TakePendingActivation`) and
  // registers the binding in order.
  void RecordPendingActivation(
      hir::ProceduralVarId hir_id, PromotedVarBinding binding) {
    pending_activation_.insert_or_assign(hir_id, binding);
  }

  [[nodiscard]] auto TakePendingActivation(hir::ProceduralVarId hir_id)
      -> PromotedVarBinding {
    const auto it = pending_activation_.find(hir_id);
    if (it == pending_activation_.end()) {
      throw InternalError(
          "ProcessLowerer::TakePendingActivation: var was not opened into an "
          "activation scope");
    }
    const PromotedVarBinding binding = it->second;
    pending_activation_.erase(it);
    return binding;
  }

  // The synthesized identifier for the callable being lowered (e.g.
  // `"process_3"`, or a method's user-given name). Used as the produced
  // method's name and as a prefix for any per-callable artifact the body
  // emits (e.g. a lifetime-extended activation scope).
  [[nodiscard]] auto CallableName() const -> std::string_view {
    return callable_name_;
  }

  // The access the produced method declares, carried onto every `MethodDecl`
  // this lowerer returns.
  [[nodiscard]] auto Visibility() const -> mir::MethodVisibility {
    return visibility_;
  }

  // The owner class's constructor-time frame -- the base each body lowering
  // extends with its own block / bindings. Carries the outer-class context
  // (self pointer type, scope chain) so a body frame derived from it
  // resolves `self` to the owner. Body lowering does NOT write into this
  // frame's block; static initializers are deferred via
  // `RecordPendingStaticInitializer` and integrated by the caller in the
  // appropriate lifecycle phase (Initialize for module-level bodies, the
  // class constructor for SV-class methods).
  [[nodiscard]] auto OwnerCtorFrame() const -> const WalkFrame& {
    return owner_ctor_frame_;
  }

  // The callable's storage plan: static placements + scope materialization
  // table for chain walks. Borrowed for the body lowering's lifetime.
  [[nodiscard]] auto StoragePlan() const -> const CallableStoragePlan& {
    return *storage_plan_;
  }

  // Resolves a HIR static-lifetime body local to the placement the planner
  // produced during shape declaration. A compiler-bug invariant if called
  // for a var with no placement (an automatic local).
  [[nodiscard]] auto LookupStaticPlacement(hir::ProceduralVarId hir_id) const
      -> StaticStoragePlacement {
    const auto placement = storage_plan_->StaticPlacement(hir_id);
    if (!placement.has_value()) {
      throw InternalError(
          "ProcessLowerer::LookupStaticPlacement: HIR procedural var has "
          "no pre-declared static placement");
    }
    return *placement;
  }

  // Records that a static declaration's initializer must run in the
  // Initialize phase. Body lowering calls this from the static-var
  // declaration handler instead of lowering the initializer itself; the
  // pending list is returned in the `LoweredCallable` for caller
  // integration. The HIR `init_expr` is kept as a handle (not pre-lowered)
  // so the dedicated initializer lowering path can lower it in the
  // Initialize phase context.
  void RecordPendingStaticInitializer(PendingStaticInitializer entry) {
    pending_static_initializers_.push_back(std::move(entry));
  }

  // Hands the accumulated pending initializers to the caller (Run extracts
  // them at the end of body lowering to fold into `LoweredCallable`).
  [[nodiscard]] auto TakePendingStaticInitializers()
      -> std::vector<PendingStaticInitializer> {
    return std::move(pending_static_initializers_);
  }

  // Builds the chained MemberAccess that reaches a static-lifetime body
  // local's storage from the current frame's `self`: starts at the
  // enclosing class pointer, walks the placement's companion chain (queried
  // from the procedural-scope materialization table), and ends with a
  // FieldAccess to the storage field. The expr's type is the storage
  // field's type read from the owner class's published shape, so an
  // observable-wrapped static returns a wrapper Expr the caller routes
  // through the cell protocol or uses as a write target.
  [[nodiscard]] auto BuildStaticStorageAccess(
      const WalkFrame& frame, StaticStoragePlacement placement) const
      -> mir::Expr;

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
      mir::Block& block, std::optional<mir::ExprId> explicit_value)
      -> std::optional<mir::ExprId>;

 private:
  ModuleLowerer* module_;
  const StructuralScopeLowerer* enclosing_scope_lowerer_;
  TimeResolution time_resolution_;
  const hir::ProceduralBody* hir_body_;
  std::string callable_name_;
  mir::MethodVisibility visibility_;
  WalkFrame owner_ctor_frame_;
  // Per-callable storage plan owned by the enclosing scope's lowerer;
  // borrowed here for the body lowering's lifetime.
  const CallableStoragePlan* storage_plan_;
  // Body-local environment bindings, indexed by HIR procedural var id;
  // nullopt for a static var (its placement lives on the storage plan).
  std::vector<std::optional<ProceduralVarBinding>> bindings_;

  // Completion-payload shape of the subroutine being lowered, set by
  // Run(subroutine) before its body walks and read by every return site. All
  // default for a process or an empty-payload callable: no result var, no pack
  // locals, so BuildReturnPayload yields a plain `return;`.
  std::optional<mir::LocalId> result_var_;
  mir::TypeId result_value_type_{};
  std::vector<mir::LocalId> output_pack_vars_;
  std::vector<mir::TypeId> output_pack_types_;
  mir::TypeId completion_payload_type_{};

  std::map<hir::ProceduralVarId, PromotedVarBinding> pending_activation_;

  // Static-initializer requests accumulated during body walk; the caller
  // pulls them out of the returned LoweredCallable and integrates them into
  // the Initialize phase block.
  std::vector<PendingStaticInitializer> pending_static_initializers_;
};

}  // namespace lyra::lowering::hir_to_mir
