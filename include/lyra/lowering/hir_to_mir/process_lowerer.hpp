#pragma once

#include <cstdint>
#include <format>
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
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

struct AutomaticVarBinding {
  // The slot's declared MIR type. A `ref` / `const ref` formal's slot is a
  // `VariableRefType`, so a reference to it reaches the place that reference
  // stands for rather than reading the slot as a value. The binding's
  // cross-body identity is its HIR procedural-var id, materialized through the
  // callable's binding context, not stored here.
  mir::TypeId type;
};

// LRM 6.21: an automatic variable -- a local, a formal, a function's result --
// a detached fork branch borrows and can outlive is promoted into a cell of its
// own, held by a shared pointer. Its reads and writes reach the cell through
// the pointer; a branch captures the pointer by value to keep the cell alive
// across the declaring frame's return.
struct PromotedVarBinding {
  BindingOriginId handle_origin;
  // The cell the handle holds, the observable cell every variable a body
  // declares is.
  mir::TypeId cell_type;
};

// The place a lifetime-extended automatic is: its cell, reached through the
// handle the body holds.
[[nodiscard]] auto PromotedVarPlace(
    const mir::TypePool& types, const WalkFrame& frame,
    const PromotedVarBinding& binding) -> mir::Expr;

// Where one HIR procedural var of this body keeps its storage: an in-frame
// local for an automatic, a cell behind a shared pointer for a
// lifetime-extended automatic, or a cell that outlives every activation for a
// static-lifetime variable (LRM 6.21). Every var the body reaches has exactly
// one of these, so a reader visits the answer rather than probing one registry
// and falling back to another.
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
  // structural scope this body counts its hops from, and is null for a body
  // that counts from none -- a callable of a namespace unit, whose names
  // resolve against the unit rather than an instance. A method of a class the
  // scope declares counts from that scope like any body of it, reaching the
  // instance through what the object records rather than through `self`.
  // `callable_name` names the artifacts the body emits, never the declaration
  // the caller wraps this code in; that declaration's own name and visibility
  // are the caller's to attach, since what is produced here is code and not a
  // declaration. `owner_ctor_frame` carries the owner class into the body --
  // a frame naming no class is a namespace context, whose body has no `self`.
  // `scopes` is what each procedural scope of the declaration scope owns at run
  // time, and `statics` the storage this body's own static variables were
  // given.
  ProcessLowerer(
      UnitLowerer& unit_lowerer,
      const StructuralScopeLowerer* enclosing_scope_lowerer,
      TimeResolution time_resolution, const hir::ProceduralBody& hir_body,
      std::optional<hir::StmtId> hir_root_stmt, WalkFrame owner_ctor_frame,
      const DeclaredScopes& scopes, std::span<const StaticVarBinding> statics)
      : owner_(&unit_lowerer),
        enclosing_scope_lowerer_(enclosing_scope_lowerer),
        time_resolution_(time_resolution),
        hir_body_(&hir_body),
        hir_root_stmt_(hir_root_stmt),
        owner_ctor_frame_(std::move(owner_ctor_frame)),
        scopes_(&scopes),
        // A body completes for no caller until it is lowered as one that does,
        // so this is the protocol every body starts with; lowering a subroutine
        // replaces it with the one its completion payload states.
        result_type_(unit_lowerer.Unit().builtins.coroutine_void) {
    // Where every static-lifetime variable of this body keeps its storage is
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

  // Lowers a HIR subroutine declaration into its callable code. Binds each
  // formal where its lifetime puts it -- the parameter, a frame variable, its
  // promoted cell, or the static cell every call shares -- so references
  // resolve, then walks the body. A function with a non-void result closes with
  // a trailing `return` of the implicit result variable. The leading `self`
  // parameter is seeded only when the body sits in an owner class (an instance
  // method, LRM 8.6); a body in a namespace context (a package function, LRM
  // 26.3) carries no receiver. Deferred static initializers are drained the
  // same way as for a process body.
  auto Run(const hir::SubroutineDecl& src) -> diag::Result<mir::CallableCode>;

  // Binds each of the constructor's input formals where its lifetime puts it,
  // appending the parameter it arrives through to `params`. Split from
  // body-statement lowering so the caller can evaluate base-constructor
  // arguments -- which LRM 8.7 orders before the body and which may reference
  // the ctor's formals -- in the same procedural-var registry the body will
  // use. A non-input formal is a compiler-bug invariant.
  auto RegisterConstructorFormals(
      const hir::SubroutineDecl& ctor, const WalkFrame& frame,
      std::vector<mir::LocalId>& params) -> diag::Result<void>;

  // Walks the constructor's body statements into `frame.current_block`,
  // assuming the formals have already been registered and any base-call
  // arguments the caller wants placed before them are already in the block.
  auto LowerConstructorBodyInto(const WalkFrame& frame) -> diag::Result<void>;

  // What a construction does first where it is handed a report (LRM 9.4.2):
  // it states the cells constructing reads, `reads`, before what the
  // construction reaches through a handle is stated as it is reached. `report`
  // is the parameter it is handed it by.
  auto BuildConstructionReport(
      const WalkFrame& frame, const hir::Reads& reads, mir::LocalId report)
      -> diag::Result<void>;

  // Central expression dispatcher. One switch over `hir::Expr::data` routing
  // each kind to its per-family handler; handlers recurse through this
  // method, so sub-expressions reach `frame` through it. A capability-wrapper
  // leaf is dereferenced, so the result is a value-typed expression naming the
  // storage the wrapper stands for.
  auto LowerExpr(const hir::Expr& expr, WalkFrame frame)
      -> diag::Result<mir::Expr>;

  // The same expression where a part of it is reached: storage it names is
  // that place, unread, and any other expression is its value.
  auto LowerAsPlace(const hir::Expr& expr, WalkFrame frame)
      -> diag::Result<mir::Expr>;

  // The same expression where nothing reads its value: a statement, a loop's
  // initializer or step. A write is then the write alone.
  auto LowerIgnoredExpr(const hir::Expr& expr, WalkFrame frame)
      -> diag::Result<mir::Expr>;

  // Dispatcher for an expression named as a part rather than read: same
  // dispatch as `LowerExpr` but peeling rather than composing, so what comes
  // out is the place that owns the value and the descent that reaches the
  // part. A capability wrapper leaf flows out as the bare wrapper, which is
  // what a destination re-roots from and what a reference binds to. Nothing is
  // appended, so a construct that only names the part -- a wait, a join of
  // nets -- asks this.
  auto LowerAccessPath(const hir::Expr& expr, WalkFrame frame)
      -> diag::Result<AccessPath>;

  // The same part as the target of a write, with the checks the write owes
  // before it lands appended where the statement is reached.
  auto LowerLhsExpr(const hir::Expr& expr, WalkFrame frame)
      -> diag::Result<AccessPath>;

  // Central statement dispatcher. One switch over `hir::Stmt::data` routing
  // each kind to its per-family handler.
  auto LowerStmt(const hir::Stmt& stmt, WalkFrame frame)
      -> diag::Result<mir::Stmt>;

  [[nodiscard]] auto HirBody() const -> const hir::ProceduralBody& {
    return *hir_body_;
  }

  // Where execution enters the body, which its owner states rather than the
  // body itself. A body nothing enters from the top -- a scope's static
  // initializers, an assertion's action arms -- names none, so asking is a
  // caller bug rather than an empty answer.
  [[nodiscard]] auto HirRootStmt() const -> hir::StmtId {
    if (!hir_root_stmt_.has_value()) {
      throw InternalError(
          "ProcessLowerer::HirRootStmt: this body is entered at the statements "
          "its owner names, not from the top");
    }
    return *hir_root_stmt_;
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

  // Whether this body stands in a structural scope it counts hops from.
  [[nodiscard]] auto HasEnclosingScope() const -> bool {
    return enclosing_scope_lowerer_ != nullptr;
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
          "ProcessLowerer::EnclosingScopeLowerer: this body counts its hops "
          "from no structural scope; a namespace unit replicates nothing, so a "
          "callable of one reaches its declarations by name");
    }
    return *enclosing_scope_lowerer_;
  }

  // The structural-subroutine tables live on the owning structural-scope pass;
  // this forwards to them so a templated call handler reaches a user subroutine
  // through the same surface on either pass class.
  [[nodiscard]] auto LookupHirSubroutine(
      hir::StructuralHops hops, std::span<const hir::OwnedChildStep> descent,
      hir::StructuralSubroutineId id) const -> const hir::SubroutineDecl& {
    return EnclosingScopeLowerer().LookupHirSubroutine(hops, descent, id);
  }
  [[nodiscard]] auto TranslateStructuralSubroutine(
      hir::StructuralHops hops, std::span<const hir::OwnedChildStep> descent,
      hir::StructuralSubroutineId id) const -> mir::Direct {
    return EnclosingScopeLowerer().TranslateStructuralSubroutine(
        hops, descent, id);
  }
  [[nodiscard]] auto ScopeAt(
      hir::StructuralHops hops,
      std::span<const hir::OwnedChildStep> descent) const
      -> const StructuralScopeLowerer& {
    return EnclosingScopeLowerer().ScopeAt(hops, descent);
  }
  template <typename Id>
  [[nodiscard]] auto RouteEnd(const WalkFrame& frame, Id id) const
      -> mir::ExprId {
    return EnclosingScopeLowerer().RouteEnd(frame, id);
  }

  [[nodiscard]] auto Resolution() const -> TimeResolution {
    return time_resolution_;
  }

  void MapProceduralVar(
      hir::ProceduralVarId hir_id, ProceduralVarBinding binding) {
    bindings_.Define(hir_id, binding);
  }

  // Where the named var keeps its storage. Every var a reference can name is
  // bound before anything reads it -- a static when the body opens, an
  // automatic formal by the signature, any other automatic by its declaration
  // -- so an unbound var is a reference reaching a declaration the walk has
  // not passed, which is a compiler bug rather than an answer.
  [[nodiscard]] auto LookupProceduralVar(hir::ProceduralVarId hir_id) const
      -> const ProceduralVarBinding& {
    const ProceduralVarBinding* binding = bindings_.Find(hir_id);
    if (binding == nullptr) {
      const std::optional<std::string>& name =
          hir_body_->procedural_vars.Get(hir_id).name;
      throw InternalError(
          std::format(
              "a reference to {} reaches it before its declaration has been "
              "passed",
              name.has_value() ? std::format("variable `{}`", *name)
                               : std::string{"a variable the lowering made"}));
    }
    return *binding;
  }

  // An activation scope is opened at block entry -- each promoted var's cell
  // and its handle are built then -- but a promoted var's binding must register
  // in HIR id order at its declaration like any other. So block entry leaves
  // the slot here and the declaration takes it back out, which is where the
  // binding lands in order.
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

  // A label no other loop of this body carries.
  [[nodiscard]] auto NextLoopLabel() -> mir::LoopLabelId {
    return mir::LoopLabelId{loop_label_count_++};
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

  // Assembles the completion-payload value a `return` should carry in the
  // subroutine being lowered: the product of the function's explicit return
  // value (or its implicit result variable when a `return` supplies none)
  // followed by each `output` / `inout` formal. A subroutine always carries
  // one, a product of no components included, so the caller never decides
  // whether a return has a value; nullopt means the body is a process, which
  // completes for no caller. `explicit_value` is the lowered `return expr`
  // operand, absent for a bare `return;`. Reads each variable where `frame`
  // stands, the way any reference to it there does.
  [[nodiscard]] auto BuildReturnPayload(
      const WalkFrame& frame, std::optional<mir::ExprId> explicit_value)
      -> std::optional<mir::ExprId>;

 private:
  // The frame a body of `src` is lowered in, in `code` and its `bindings`,
  // with what it takes ahead of its formals bound.
  auto EnterSubroutineBody(
      const hir::SubroutineDecl& src, mir::CallableCode& code,
      CallableBindings& bindings) -> BoundImplicitParameters;

  // A form of the subroutine `src`: what it takes ahead of its formals, its
  // formals and its result, then whatever `after_formals` adds to the
  // parameters and to the frame the body is lowered in, then the body.
  template <typename AfterFormals>
  auto LowerSubroutine(
      const hir::SubroutineDecl& src, AfterFormals after_formals)
      -> diag::Result<mir::CallableCode>;

  // What the form of a function a waited evaluation runs does first with the
  // report it is handed: for an implicit list being collected (LRM 9.2.2.2.1)
  // it states what its text reads and writes and what the functions it calls
  // do, and returns the defaults its result and outputs start at; for a waited
  // evaluation (LRM 9.4.2) it states the cells its text reads, and its body
  // then states what it reaches through a handle as it reaches it.
  auto BuildRecordingPrologue(
      const WalkFrame& frame, const hir::Reads& reads, mir::LocalId report)
      -> diag::Result<void>;

  // What the form every call runs does first: where it is handed a report, it
  // answers with what `recording`, the form that states what it reads, answers
  // for `params`, which are all of its own.
  void ForwardToRecording(
      const WalkFrame& frame, const mir::CallableCode& code,
      std::span<const mir::LocalId> params, mir::LocalId report,
      mir::DirectTarget recording);

  // States the cells of `reads` into `report`. Where they cannot be stated,
  // the design fails here, at the wait that asked.
  auto ReportCellsRead(
      const WalkFrame& frame, const hir::Reads& reads, mir::LocalId report)
      -> diag::Result<void>;

  UnitLowerer* owner_;
  const StructuralScopeLowerer* enclosing_scope_lowerer_;
  TimeResolution time_resolution_;
  const hir::ProceduralBody* hir_body_;
  std::optional<hir::StmtId> hir_root_stmt_;
  WalkFrame owner_ctor_frame_;
  // Owned by the enclosing declaration scope's lowerer; borrowed here for the
  // body lowering's lifetime.
  const DeclaredScopes* scopes_;
  base::SymbolTable<hir::ProceduralVarId, ProceduralVarBinding> bindings_;

  // The result type of the body being lowered, set before its body walks. It
  // is the call protocol, so every return site reads what its completion
  // carries from it -- a body that completes for no caller carries nothing,
  // and one that completes for a caller carries its completion payload. What
  // fills that payload is the result variable, if the body has one, followed
  // by each output / inout formal.
  mir::TypeId result_type_;
  std::optional<hir::ProceduralVarId> result_var_;
  std::vector<hir::ProceduralVarId> output_vars_;

  std::map<hir::ProceduralVarId, PromotedVarBinding> pending_activation_;
  std::uint32_t loop_label_count_ = 0;
};

}  // namespace lyra::lowering::hir_to_mir
