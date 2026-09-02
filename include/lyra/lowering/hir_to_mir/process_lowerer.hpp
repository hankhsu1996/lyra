#pragma once

#include <map>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/symbol_table.hpp"
#include "lyra/base/time.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

struct AutomaticVarBinding {
  // The slot's declared MIR type. A `ref` / `const ref` formal's slot is a
  // `RefType`, so a reference to it reaches the place that reference stands for
  // rather than reading the slot as a value. The binding's
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

// Where one HIR procedural var of this body keeps its storage: an in-frame
// local for an automatic, a field of a shared activation object for a
// lifetime-extended automatic (LRM 6.21), or a field that outlives every
// activation for a static-lifetime local (LRM 13.3.1). Every var the body
// reaches has exactly one of these, so a reader visits the answer rather than
// probing one registry and falling back to another.
using ProceduralVarBinding =
    std::variant<AutomaticVarBinding, PromotedVarBinding, StaticVarBinding>;

// Lowers one procedural body -- a process, a method, a subroutine, a
// constructor -- into its callable code. What holds for the whole body sits
// here: where it lives, what it may name, where each of its vars keeps its
// storage. What differs between one point of the walk and another -- the block
// being written, the class in scope, the bindings a reference resolves against
// -- travels down the walk instead, so nothing here changes as the walk moves.
class ProcessLowerer {
 public:
  // `enclosing_scope_lowerer` resolves every reference to a declaration of the
  // structural scope this body sits inside, and is null for a body that sits
  // inside none -- a class method reaches its owner through `self` instead.
  // `callable_name` names the artifacts the body emits, never the declaration
  // the caller wraps this code in; that declaration's own name and visibility
  // are the caller's to attach, since what is produced here is code and not a
  // declaration. `owner_ctor_frame` carries the owner class into the body --
  // a frame naming no class is a namespace context, whose body has no `self`.
  // `scopes` is what each procedural scope of the declaration scope owns at run
  // time, and `statics` the storage this body's own static locals were given.
  ProcessLowerer(
      UnitLowerer& unit_lowerer,
      const StructuralScopeLowerer* enclosing_scope_lowerer,
      TimeResolution time_resolution, const hir::ProceduralBody& hir_body,
      std::string callable_name, WalkFrame owner_ctor_frame,
      const DeclaredScopes& scopes, std::span<const StaticVarBinding> statics)
      : owner_(&unit_lowerer),
        enclosing_scope_lowerer_(enclosing_scope_lowerer),
        time_resolution_(time_resolution),
        hir_body_(&hir_body),
        callable_name_(std::move(callable_name)),
        owner_ctor_frame_(std::move(owner_ctor_frame)),
        scopes_(&scopes),
        // A body completes for no caller until it is lowered as one that does,
        // so this is the protocol every body starts with; lowering a subroutine
        // replaces it with the one its completion payload states.
        result_type_(unit_lowerer.Unit().builtins.coroutine_void) {
    // Where every static-lifetime local of this body keeps its storage is
    // settled before the body lowers, so each one is bound here. Reaching a
    // static's declaration statement then binds nothing and emits nothing --
    // the walk carries no part of the answer.
    for (const StaticVarBinding& binding : statics) {
      MapProceduralVar(binding.var, binding);
    }
  }

  // Lowers an entire HIR process (initial / final / always / always_ff /
  // always_comb / always_latch) into its callable code -- a coroutine body the
  // caller registers.
  auto Run(const hir::Process& src) -> diag::Result<mir::CallableCode>;

  // Lowers a HIR subroutine declaration into its callable code. Pre-registers
  // the formal params as body locals so call references resolve, then walks the
  // body. A function with a non-void result closes with a trailing `return` of
  // the implicit result variable. The leading `self` parameter is seeded only
  // when the body sits in an owner class (an instance method, LRM 8.6); a body
  // in a namespace context (a package function, LRM 26.3) carries no receiver.
  // Deferred static initializers are drained the same way as for a process
  // body.
  auto Run(const hir::SubroutineDecl& src) -> diag::Result<mir::CallableCode>;

  // Registers each of the constructor's input formals as a MIR body local,
  // appending its `LocalId` to `params`. Split from body-statement lowering
  // so the caller can evaluate base-constructor arguments -- which LRM 8.7
  // orders before the body and which may reference the ctor's formals --
  // in the same procedural-var registry the body will use. A non-input
  // formal is a compiler-bug invariant.
  auto RegisterConstructorFormals(
      const hir::SubroutineDecl& ctor, const WalkFrame& frame,
      std::vector<mir::LocalId>& params) -> diag::Result<void>;

  // Walks the constructor's body statements into `frame.current_block`,
  // assuming the formals have already been registered and any base-call
  // arguments the caller wants placed before them are already in the block.
  auto LowerConstructorBodyInto(const WalkFrame& frame) -> diag::Result<void>;

  // Central expression dispatcher. One switch over `hir::Expr::data` routing
  // each kind to its per-family handler; handlers recurse through this
  // method, so sub-expressions reach `frame` through it. A capability-wrapper
  // leaf is dereferenced, so the result is a value-typed expression naming the
  // storage the wrapper stands for.
  auto LowerExpr(const hir::Expr& expr, WalkFrame frame)
      -> diag::Result<mir::Expr>;

  // LHS-context expression dispatcher: same dispatch as `LowerExpr` but
  // without that dereference, so a capability-wrapper leaf flows out as the
  // bare wrapper -- which is what a destination re-roots from and what a
  // reference binds to.
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

  // The pattern arena of the body being lowered, exposed with the same shape
  // on both pass classes for the same reason the expression arena is.
  [[nodiscard]] auto HirPatterns() const
      -> const base::Arena<hir::Pattern, hir::PatternId>& {
    return hir_body_->patterns;
  }

  [[nodiscard]] auto Owner() -> UnitLowerer& {
    return *owner_;
  }
  [[nodiscard]] auto Owner() const -> const UnitLowerer& {
    return *owner_;
  }

  // The lowering pass for the structural scope this body sits inside; its
  // registries resolve every reference to an enclosing-scope declaration (a
  // structural variable, a generate loop variable, a cross-unit reference, a
  // peer subroutine). Absent for a body that sits inside no structural scope
  // -- a class method or a package callable -- because the source forms
  // resolved through this pass are meaningless there: a class method reaches
  // its owner through `self`, and a package callable reaches its peers by
  // name across the unit boundary. Reaching this from such a body is a
  // compiler bug.
  [[nodiscard]] auto EnclosingScopeLowerer() const
      -> const StructuralScopeLowerer& {
    if (enclosing_scope_lowerer_ == nullptr) {
      throw InternalError(
          "ProcessLowerer::EnclosingScopeLowerer: this body has no enclosing "
          "structural scope; reaching an enclosing structural declaration from "
          "a package callable or a class method body is a compiler bug");
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
  [[nodiscard]] auto RoutedRefTarget(hir::RoutedRefId hir_id) const
      -> const RoutedRefMeta& {
    return EnclosingScopeLowerer().RoutedRefTarget(hir_id);
  }

  [[nodiscard]] auto Resolution() const -> TimeResolution {
    return time_resolution_;
  }

  void MapProceduralVar(
      hir::ProceduralVarId hir_id, ProceduralVarBinding binding) {
    bindings_.Define(hir_id, binding);
  }

  // Where the named var keeps its storage. Every var a reference can name is
  // bound before anything reads it -- a static when the body opens, a formal by
  // the signature, an automatic by its declaration -- so an unbound var is a
  // reference reaching a declaration the walk has not passed, which is a
  // compiler bug rather than an answer.
  // What binds a var is not its HIR lifetime: a subroutine's formals and its
  // result variable are bound by the signature whatever lifetime their
  // declarations report, which is why a reader asks here rather than reading
  // the declaration.
  [[nodiscard]] auto LookupProceduralVar(hir::ProceduralVarId hir_id) const
      -> const ProceduralVarBinding& {
    return bindings_.Get(hir_id);
  }

  // An activation scope is opened at block entry -- the scope struct and its
  // handle are built then -- but a promoted var's binding must register in HIR
  // id order at its declaration like any other. So block entry leaves the slot
  // here and the declaration takes it back out, which is where the binding
  // lands in order.
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

  // The synthesized identifier for the callable being lowered (`"process_3"`,
  // or a user-given name), used as a prefix for any per-callable artifact the
  // body emits (e.g. a lifetime-extended activation scope's struct).
  [[nodiscard]] auto CallableName() const -> std::string_view {
    return callable_name_;
  }

  // The owner class's constructor-time frame -- the base each body lowering
  // extends with its own block / bindings. Carries the outer-class context
  // (self pointer type, scope chain) so a body frame derived from it
  // resolves `self` to the owner. Body lowering does NOT write into this
  // frame's block; a static's declaration assignment is not an effect of the
  // body at all, and the caller performs it in the appropriate lifecycle phase
  // (Initialize for module-level bodies, the class constructor for SV-class
  // methods).
  [[nodiscard]] auto OwnerCtorFrame() const -> const WalkFrame& {
    return owner_ctor_frame_;
  }

  // What each procedural scope of the enclosing declaration scope owns at run
  // time. Borrowed for the body lowering's lifetime.
  [[nodiscard]] auto Scopes() const -> const DeclaredScopes& {
    return *scopes_;
  }

  // This body's own root scope. A task or function is a scope the source named
  // (LRM 23.9), so a body starts in one; a scope entered below replaces it.
  [[nodiscard]] auto RootScope() const -> const DeclaredScope& {
    return scopes_->Get(hir_body_->root_scope);
  }

  // Whether the body being lowered reaches an object of its own. Per-instance
  // storage is projected from that object, so a body without one -- a package
  // callable (LRM 26.3) or a static class method (LRM 8.10) -- can reach none.
  [[nodiscard]] auto BodyHasReceiver() const -> bool {
    return body_has_receiver_;
  }

  // Assembles the completion-payload value a `return` should carry in the
  // subroutine being lowered: the product of the function's explicit return
  // value (or its implicit result variable when a `return` supplies none)
  // followed by each `output` / `inout` local. A subroutine always carries one,
  // a product of no components included, so the caller never decides whether a
  // return has a value; nullopt means the body is a process, which completes
  // for no caller. `explicit_value` is the lowered `return expr` operand,
  // absent for a bare `return;`. Reads the pack locals at `frame`'s depth, so a
  // return nested in an inner block resolves the correct hops.
  [[nodiscard]] auto BuildReturnPayload(
      mir::Block& block, std::optional<mir::ExprId> explicit_value)
      -> std::optional<mir::ExprId>;

 private:
  UnitLowerer* owner_;
  const StructuralScopeLowerer* enclosing_scope_lowerer_;
  TimeResolution time_resolution_;
  const hir::ProceduralBody* hir_body_;
  std::string callable_name_;
  WalkFrame owner_ctor_frame_;
  // Owned by the enclosing declaration scope's lowerer; borrowed here for the
  // body lowering's lifetime.
  const DeclaredScopes* scopes_;
  // A process body always runs on the scope object that owns it; a subroutine
  // body sets this from its own form when it lowers.
  bool body_has_receiver_ = true;
  base::SymbolTable<hir::ProceduralVarId, ProceduralVarBinding> bindings_;

  // The result type of the body being lowered, set before its body walks. It
  // is the call protocol, so every return site reads what its completion
  // carries from it -- a body that completes for no caller carries nothing,
  // and one that completes for a caller carries its completion payload. What
  // fills that payload is the result variable, if the body has one, followed
  // by each output / inout local.
  mir::TypeId result_type_;
  std::optional<mir::LocalId> result_var_;
  mir::TypeId result_value_type_{};
  std::vector<mir::LocalId> output_pack_vars_;
  std::vector<mir::TypeId> output_pack_types_;

  std::map<hir::ProceduralVarId, PromotedVarBinding> pending_activation_;
};

}  // namespace lyra::lowering::hir_to_mir
