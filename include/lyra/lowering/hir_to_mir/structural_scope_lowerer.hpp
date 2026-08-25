#pragma once

#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/declared_callable.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/package_initialization.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

struct ChildStructuralScopeBinding {
  // The child's SV-visible label. It is the identity the child object is
  // built with, and the key a by-name descent matches it on.
  std::string label;
  // The borrowed typed handle on the parent's class that a route step
  // navigates through to reach this child.
  mir::FieldId borrowed_handle;
  // The child's own lowerer, which carries the class it lowers to and resolves
  // the identities a route step past this child names.
  const StructuralScopeLowerer* lowerer = nullptr;
};

// How a hierarchical route reaches an owned child: the parent's borrowed typed
// handle on it, and the child's own lowerer. The handle's type carries the
// child's cardinality, so a step reaches an element by indexing it rather than
// by naming the child a second time. `target_scope` is present when the
// artifact owns the child's body -- so the receiver stays typed and whatever
// the route names next resolves against that scope -- and absent when the
// child is another compilation unit, opaque from there.
struct OwnedChildAnchor {
  mir::FieldId borrowed_handle{};
  const StructuralScopeLowerer* target_scope = nullptr;
};

// What a single `hir::Generate` settled for each block it elaborated, reached
// by that block's own id.
using GenerateBindings =
    base::Translation<hir::StructuralScopeId, ChildStructuralScopeBinding>;

// The MIR slot a HIR routed ref resolves to: the slot's field id bundled
// with the slot's MIR type, so a body reader decides whether the read
// dereferences without re-touching the owning scope.
struct RoutedRefMeta {
  mir::FieldId target = {};
  mir::TypeId slot_type = {};
};

// Lowers one HIR structural scope into one MIR class, in two passes over the
// scope tree: the first settles what every scope declares, the second lowers
// every body against those. Two, because a body may name any peer's
// declaration while no declaration names a body. Each scope's lowering holds
// what it settled and borrows the enclosing one, so a reference climbing out
// resolves against the scope it names; and the tree of them outlives the first
// pass, which is what the second reads. The class being built is not held here
// -- a body reaches it down the walk, like everything else that changes as the
// walk moves.
class StructuralScopeLowerer {
 public:
  StructuralScopeLowerer(
      UnitLowerer& unit_lowerer, const StructuralScopeLowerer* parent,
      std::string name, const hir::StructuralScope& hir_scope,
      PackageInitializationPlan package_init_plan = {})
      : owner_(&unit_lowerer),
        parent_(parent),
        name_(std::move(name)),
        hir_scope_(&hir_scope),
        package_init_plan_(std::move(package_init_plan)) {
  }

  // Mints this class's identity, builds its structural shape, publishes the
  // shape so peer body lowering can query it, and recurses to declare every
  // descendant scope's shape.
  auto DeclareShape() -> diag::Result<mir::ClassId>;

  // Lowers every body and every install statement against the already-
  // published shape, recurses into descendants, and commits the composed
  // class to the compilation unit. `parent_frame` carries the
  // enclosing-class chain this scope's bodies thread through; the root call
  // receives a default `WalkFrame`.
  auto PopulateBodies(WalkFrame parent_frame) -> diag::Result<void>;

  // Central scope-level expression dispatcher. One switch over `hir::Expr::
  // data` routing each kind to its per-family handler.
  [[nodiscard]] auto LowerExpr(const hir::Expr& expr, WalkFrame frame) const
      -> diag::Result<mir::Expr>;

  // LHS-context expression dispatcher: addressable kinds only, no auto-Get
  // wrap.
  [[nodiscard]] auto LowerLhsExpr(const hir::Expr& expr, WalkFrame frame) const
      -> diag::Result<mir::Expr>;

  [[nodiscard]] auto Owner() const -> UnitLowerer& {
    return *owner_;
  }

  [[nodiscard]] auto Parent() const -> const StructuralScopeLowerer* {
    return parent_;
  }

  [[nodiscard]] auto Name() const -> const std::string& {
    return name_;
  }

  [[nodiscard]] auto HirScope() const -> const hir::StructuralScope& {
    return *hir_scope_;
  }

  // The scope's time unit (LRM 3.14.2), which a time query scales its result
  // to. Named as the procedural pass names it, so a handler shared by both
  // passes reads it the same way.
  [[nodiscard]] auto Resolution() const -> TimeResolution {
    return hir_scope_->time_resolution;
  }

  // The expression arena of the scope being lowered. The uniform sub-expression
  // accessor the context-free expression handler templates reach through; both
  // lowering pass classes expose it with the same shape so those templates bind
  // to either.
  [[nodiscard]] auto HirExprs() const
      -> const base::Arena<hir::Expr, hir::ExprId>& {
    return hir_scope_->exprs;
  }

  // The pattern arena of the scope being lowered, exposed with the same shape
  // on both pass classes for the same reason the expression arena is.
  [[nodiscard]] auto HirPatterns() const
      -> const base::Arena<hir::Pattern, hir::PatternId>& {
    return hir_scope_->patterns;
  }

  // Resolve a subroutine reference to its HIR declaration by walking `hops`
  // scopes outward. The HIR declaration is complete before any body is lowered,
  // so a call can read a peer's formals even when the peer's MIR declaration is
  // not yet built (forward / mutual reference, LRM 13.7). The desugar reads the
  // formals' directions and types from here.
  [[nodiscard]] auto LookupHirSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId id) const
      -> const hir::SubroutineDecl& {
    if (hops.value == 0) {
      return hir_scope_->structural_subroutines.Get(id);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::LookupHirSubroutine: hops walk ran "
          "past the root scope");
    }
    return parent_->LookupHirSubroutine(
        hir::StructuralHops{.value = hops.value - 1}, id);
  }

  [[nodiscard]] auto RoutedRefTarget(hir::RoutedRefId hir_id) const
      -> const RoutedRefMeta& {
    return routed_ref_targets_.Get(hir_id);
  }

  // The scope `hops` enclosing edges out from this one, in the same
  // compilation unit. A route anchored there resolves each identity it names
  // against that scope, the same reach a sibling or child route uses.
  [[nodiscard]] auto EnclosingScopeAtHops(hir::StructuralHops hops) const
      -> const StructuralScopeLowerer& {
    if (hops.value == 0) {
      return *this;
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::EnclosingScopeAtHops: hops walk ran past "
          "the root scope");
    }
    return parent_->EnclosingScopeAtHops(
        hir::StructuralHops{.value = hops.value - 1});
  }

  // The MIR field a structural data object became, in the scope `hops`
  // enclosing edges out from this one.
  [[nodiscard]] auto TranslateStructuralDataObject(
      hir::StructuralHops hops, hir::StructuralDataObjectId hir_id) const
      -> mir::FieldId {
    if (hops.value == 0) {
      return data_object_fields_.Get(hir_id);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateStructuralDataObject: hops out "
          "of scope chain");
    }
    return parent_->TranslateStructuralDataObject(
        hir::StructuralHops{hops.value - 1}, hir_id);
  }

  // Resolves an owned-child reference to how the route reaches it: the
  // parent's borrowed handle on it, and the child's own lowerer when the
  // artifact owns the child's body. `hops == 0` reads this scope's own tables;
  // `hops > 0` walks the parent chain to an enclosing scope, used by the
  // sibling-of-ancestor install when the child lives outside the referrer's
  // frame.
  [[nodiscard]] auto TranslateOwnedChild(
      hir::StructuralHops hops, const hir::OwnedChildRef& child) const
      -> OwnedChildAnchor {
    if (hops.value == 0) {
      return std::visit(
          Overloaded{
              [&](const hir::InstanceMemberId& id) -> OwnedChildAnchor {
                // A module instance's body is another compilation unit, so
                // this artifact lowers no scope for it; the member is typed but
                // opaque from there.
                return OwnedChildAnchor{
                    .borrowed_handle = InstanceBorrowedHandle(id),
                    .target_scope = nullptr};
              },
              [&](const hir::GenerateChildRef& g) -> OwnedChildAnchor {
                const auto& b = generate_bindings_.Get(g.generate).Get(g.scope);
                return OwnedChildAnchor{
                    .borrowed_handle = b.borrowed_handle,
                    .target_scope = b.lowerer};
              },
          },
          child);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateOwnedChild: hops exceed scope "
          "chain depth");
    }
    return parent_->TranslateOwnedChild(
        hir::StructuralHops{hops.value - 1}, child);
  }

  // Registry identity of the class this scope lowers to.
  [[nodiscard]] auto ClassId() const -> mir::ClassId {
    return class_id_;
  }

  // The storage one of this scope's static-lifetime body locals was given, and
  // the scope that names it. A reference names the declaration rather than the
  // blocks around it, and the storage is this class's own field, so a
  // referrer standing on this object is already standing on the cell.
  [[nodiscard]] auto ProceduralStaticBinding(
      const hir::ProceduralBodyRef& body, hir::ProceduralVarId var) const
      -> StaticVarBinding {
    const StaticVarBindings& statics = std::visit(
        Overloaded{
            [&](hir::ProcessId id) -> const StaticVarBindings& {
              return process_static_bindings_.Get(id);
            },
            [&](hir::StructuralSubroutineId id) -> const StaticVarBindings& {
              return declared_subroutines_.Get(id).statics;
            }},
        body);
    for (const StaticVarBinding& binding : statics) {
      if (binding.var == var) return binding;
    }
    throw InternalError(
        "StructuralScopeLowerer::ProceduralStaticBinding: the var was given no "
        "persistent storage, so it is not a static-lifetime local of that "
        "body");
  }

  // What each of this scope's procedural scopes owns at run time.
  [[nodiscard]] auto Scopes() const -> const DeclaredScopes& {
    return scopes_;
  }

  // The call target a structural subroutine reference resolves to: the class
  // that owns the callable, `hops` enclosing edges out from this one, and the
  // callable's identity within it.
  [[nodiscard]] auto TranslateStructuralSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId hir_id) const
      -> mir::Direct {
    if (hops.value == 0) {
      return mir::Direct{
          .target = mir::CallableTarget{
              .owner = class_id_,
              .slot = declared_subroutines_.Get(hir_id).callable}};
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateStructuralSubroutine: hops "
          "exceed scope chain depth");
    }
    return parent_->TranslateStructuralSubroutine(
        hir::StructuralHops{hops.value - 1}, hir_id);
  }

  // The handle this scope's class keeps on one instance member, which a route
  // step navigates through to reach it.
  [[nodiscard]] auto InstanceBorrowedHandle(hir::InstanceMemberId hir_id) const
      -> mir::FieldId {
    return instance_borrowed_handles_.Get(hir_id);
  }

 private:
  UnitLowerer* owner_;
  const StructuralScopeLowerer* parent_;
  std::string name_;
  const hir::StructuralScope* hir_scope_;
  // Non-empty only on the design root's own scope, the sole scope whose
  // elaboration spans the whole design. Every source unit's scope and every
  // nested scope leaves it empty.
  PackageInitializationPlan package_init_plan_;
  base::Translation<hir::StructuralDataObjectId, mir::FieldId>
      data_object_fields_;
  base::Translation<hir::RoutedRefId, RoutedRefMeta> routed_ref_targets_;
  base::Translation<hir::GenerateId, GenerateBindings> generate_bindings_;
  base::Translation<hir::InstanceMemberId, mir::FieldId>
      instance_borrowed_handles_;
  DeclaredScopes scopes_;
  base::Translation<hir::StructuralSubroutineId, DeclaredCallable>
      declared_subroutines_;
  // A process is anonymous (LRM 9.2), so nothing can name it early and it takes
  // no callable identity: its answer is storage alone, where a subroutine's is
  // a whole declared callable.
  base::Translation<hir::ProcessId, StaticVarBindings> process_static_bindings_;
  mir::ClassId class_id_{};
  std::vector<std::unique_ptr<StructuralScopeLowerer>> children_;
};

}  // namespace lyra::lowering::hir_to_mir
