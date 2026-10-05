#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <span>
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
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/declared_callable.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/design_namespaces.hpp"
#include "lyra/lowering/hir_to_mir/object_change.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

struct ChildStructuralScopeBinding {
  // The child's SV-visible label, the identity the child object is built with.
  std::string label;
  // The child's own lowerer, which carries the class it lowers to and resolves
  // the identities a route step past this child names.
  const StructuralScopeLowerer* lowerer = nullptr;
  // The arguments building the child passes its constructor, as expressions of
  // the scope building it.
  std::span<const hir::ExprId> arguments;
};

// What one generate construct settled: the member holding what it built, and
// each scope it compiled to, reached by that scope's own id. The member holds
// the base every scope extends -- one, or a sequence of them for a loop --
// because which class a block was built as is the block's to say, and a step
// reaching one views it as that class.
struct GenerateBinding {
  mir::ClassFieldTarget handle;
  base::Translation<hir::StructuralScopeId, ChildStructuralScopeBinding> blocks;
};

// A value whoever builds a scope hands it: the declaration it fills, and the
// type the constructor takes it as.
struct ConstructionValue {
  hir::StructuralDataObjectId declared;
  mir::TypeId type;
};

// One subroutine a unit published: the identifier a referrer spells and the
// body of the realizing class that carries it out. The published class states
// a method under that identifier, entering the body on the object.
struct PublishedSubroutine {
  std::string name;
  mir::CallableId body;
};

// How a hierarchical route reaches an owned child: the parent's borrowed
// handle on it, and the child's own lowerer. The handle's type carries the
// declaration's multiplicity, so each select of the element naming the child
// indexes the handle. `target_scope` is present when the artifact owns the
// child's body -- the handle then holds the base every scope extends, the step
// views what it reached as that scope's class, and whatever the route names
// next resolves against that scope -- and absent when the child is another
// compilation unit, whose handle is typed as that unit's class already.
struct OwnedChildAnchor {
  mir::ClassFieldTarget borrowed_handle{};
  const StructuralScopeLowerer* target_scope = nullptr;
};

// A route made only of parent edges within this unit, `hops` of them. Every
// scope it passes encloses the reader and so exists whenever the reader does,
// which leaves nothing about it to settle: it is walked where it is used.
struct ClimbedRoute {
  hir::StructuralHops hops;
};

// A route that descends into a scope or crosses into another unit, which may
// reach something not built yet, not selected, or answered only by the
// runtime. It is walked once the tree is whole, and what it reached is kept in
// `slot`, a member of the reader's own class.
struct StoredRoute {
  mir::FieldId slot;
};

// How a route is reached from its reader.
using RouteReach = std::variant<ClimbedRoute, StoredRoute>;

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
      const hir::StructuralScope& hir_scope, DesignNamespaces namespaces = {})
      : owner_(&unit_lowerer),
        parent_(parent),
        hir_scope_(&hir_scope),
        namespaces_(std::move(namespaces)) {
  }

  // Mints this class's identity, builds its structural shape, publishes the
  // shape so peer body lowering can query it, and recurses to declare every
  // descendant scope's shape.
  auto DeclareShape() -> diag::Result<mir::ClassId>;

  // The class the unit published of the object this scope is.
  [[nodiscard]] auto PublishedClassId() const -> mir::ClassId {
    return published_class_id_;
  }

  // The values whoever builds this scope hands it, in the order they are
  // handed.
  [[nodiscard]] auto ConstructionValues() const
      -> std::span<const ConstructionValue> {
    return construction_values_;
  }

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

  // The same expression where nothing reads its value: a loop generate's step.
  // A write is then the write alone.
  [[nodiscard]] auto LowerIgnoredExpr(
      const hir::Expr& expr, WalkFrame frame) const -> diag::Result<mir::Expr>;

  // Dispatcher for an expression named as a part rather than read: addressable
  // kinds only, no auto-Get wrap, peeled into the place that owns the value and
  // the descent above it. Nothing is appended, so a construct that only names
  // the part -- a wait, a join of nets -- asks this.
  [[nodiscard]] auto LowerAccessPath(
      const hir::Expr& expr, WalkFrame frame) const -> diag::Result<AccessPath>;

  // The same part as the target of a write, with the checks the write owes
  // before it lands appended where the statement is reached.
  [[nodiscard]] auto LowerLhsExpr(const hir::Expr& expr, WalkFrame frame) const
      -> diag::Result<AccessPath>;

  [[nodiscard]] auto Owner() const -> UnitLowerer& {
    return *owner_;
  }

  [[nodiscard]] auto Parent() const -> const StructuralScopeLowerer* {
    return parent_;
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
      hir::StructuralHops hops, std::span<const hir::OwnedChildStep> descent,
      hir::StructuralSubroutineId id) const -> const hir::SubroutineDecl& {
    return ScopeAt(hops, descent).hir_scope_->structural_subroutines.Get(id);
  }

  // The scope a reach lands on: `hops` enclosing edges out, then one owned
  // child per descent step. Every step is one this unit declares, so the walk
  // is total -- a reach that leaves the layout never reaches here. Which scope
  // a step lands on is the construct's own answer about the block the step
  // names.
  [[nodiscard]] auto ScopeAt(
      hir::StructuralHops hops,
      std::span<const hir::OwnedChildStep> descent) const
      -> const StructuralScopeLowerer& {
    const StructuralScopeLowerer* scope = &EnclosingScopeAtHops(hops);
    for (const hir::OwnedChildStep& step : descent) {
      const OwnedChildAnchor anchor =
          scope->TranslateOwnedChild(step.names, step.selects);
      if (anchor.target_scope == nullptr) {
        throw InternalError(
            "StructuralScopeLowerer::ScopeAt: a descent step reached an "
            "object this unit does not lay out");
      }
      scope = anchor.target_scope;
    }
    return *scope;
  }

  // How each of this scope's routes, of every use, is reached from it.
  [[nodiscard]] auto ReachOf(hir::RoutedValueRefId hir_id) const
      -> const RouteReach& {
    return value_reaches_.Get(hir_id);
  }
  [[nodiscard]] auto ReachOf(hir::RoutedObjectRefId hir_id) const
      -> const RouteReach& {
    return object_reaches_.Get(hir_id);
  }
  [[nodiscard]] auto ReachOf(hir::RoutedDisableTargetRefId hir_id) const
      -> const RouteReach& {
    return disable_target_reaches_.Get(hir_id);
  }

  // What a route ends at, as the reader at `frame` reaches it -- a borrowed
  // pointer to it: the route walked there, or the slot it was kept in. Appends
  // to `frame.current_block`. A value is read through an endpoint instead,
  // which also knows what kind of member the route ends at.
  [[nodiscard]] auto RouteEnd(
      const WalkFrame& frame, hir::RoutedObjectRefId id) const -> mir::ExprId;
  [[nodiscard]] auto RouteEnd(
      const WalkFrame& frame, hir::RoutedDisableTargetRefId id) const
      -> mir::ExprId;

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

  // The MIR field the history of one sampled expression became (LRM 16.9.3).
  // It takes no hop count: a history is recorded on the scope whose process
  // reads it, so the reader and the storage are always the same scope.
  [[nodiscard]] auto TranslateSampledHistory(hir::SampledHistoryId hir_id) const
      -> mir::FieldId {
    return sampled_history_fields_.Get(hir_id);
  }

  // The MIR field holding one concurrent assertion's attempts (LRM 16.14.1).
  // It takes no hop count for the same reason: an assertion is evaluated on the
  // scope that declares it.
  [[nodiscard]] auto TranslateConcurrentAssertion(
      hir::ConcurrentAssertionId hir_id) const -> mir::FieldId {
    return concurrent_assertion_fields_.Get(hir_id);
  }

  // The MIR field a structural data object became, in the scope `hops`
  // enclosing edges out from this one: a field of the published class where
  // the unit published it, and of this scope's own class otherwise.
  [[nodiscard]] auto TranslateStructuralDataObject(
      hir::StructuralHops hops, hir::StructuralDataObjectId hir_id) const
      -> mir::ClassFieldTarget {
    if (hops.value == 0) {
      return data_object_fields_.Get(hir_id);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateStructuralDataObject: hops walk "
          "ran past the root scope");
    }
    return parent_->TranslateStructuralDataObject(
        hir::StructuralHops{hops.value - 1}, hir_id);
  }

  // The MIR field an interface port became, in the scope `hops` enclosing
  // edges out from this one.
  [[nodiscard]] auto TranslateInterfacePort(
      hir::StructuralHops hops, hir::InterfacePortId hir_id) const
      -> mir::ClassFieldTarget {
    if (hops.value == 0) {
      return interface_port_fields_.Get(hir_id);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateInterfacePort: hops walk ran past "
          "the root scope");
    }
    return parent_->TranslateInterfacePort(
        hir::StructuralHops{hops.value - 1}, hir_id);
  }

  // Resolves a path element naming one of this scope's owned children to how
  // the route reaches it: this scope's borrowed handle on it, and the child's
  // own lowerer when the artifact owns the child's body.
  [[nodiscard]] auto TranslateOwnedChild(
      const hir::OwnedChildRef& names,
      std::span<const std::uint32_t> selects) const -> OwnedChildAnchor {
    // A path names a block; which compiled scope that is, is the construct's
    // own answer.
    const auto block_of = [&](hir::GenerateId generate,
                              hir::StructuralScopeId scope) {
      const GenerateBinding& binding = generate_bindings_.Get(generate);
      return OwnedChildAnchor{
          .borrowed_handle = binding.handle,
          .target_scope = binding.blocks.Get(scope).lowerer};
    };
    return std::visit(
        Overloaded{
            [&](const hir::InstanceMemberId& id) -> OwnedChildAnchor {
              // A module instance's body is another compilation unit, so this
              // artifact lowers no scope for it; what the object holds is
              // reached through what that unit published.
              return OwnedChildAnchor{
                  .borrowed_handle = instance_member_fields_.Get(id),
                  .target_scope = nullptr};
            },
            [&](const hir::GenerateLoopRef& loop) -> OwnedChildAnchor {
              return block_of(
                  loop.generate,
                  hir::LoopBlockScopeOf(
                      HirScope().generates.Get(loop.generate), selects));
            },
            [&](const hir::GenerateBlockRef& block) -> OwnedChildAnchor {
              return block_of(
                  block.generate, hir::ChosenBlockScopeOf(
                                      HirScope().generates.Get(block.generate),
                                      block.alternative));
            },
        },
        names);
  }

  // Registry identity of the class this scope lowers to.
  [[nodiscard]] auto ClassId() const -> mir::ClassId {
    return class_id_;
  }

  // The field one of this scope's static-lifetime body locals was given. A
  // reference names the declaration rather than the procedural scopes around
  // it, and the storage is a field of this scope's object, so a referrer
  // standing on this object is already standing on the cell.
  [[nodiscard]] auto ProceduralStaticField(
      const hir::ProceduralBodyRef& body, hir::ProceduralVarId var) const
      -> mir::ClassFieldTarget {
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
      if (binding.var == var) return InstanceFieldOf(binding);
    }
    throw InternalError(
        "StructuralScopeLowerer::ProceduralStaticField: the var was given no "
        "persistent storage, so it is not a static-lifetime local of that "
        "body");
  }

  // What each of this scope's procedural scopes owns at run time.
  [[nodiscard]] auto Scopes() const -> const DeclaredScopes& {
    return scopes_;
  }

  // The field carrying what a `disable` naming one of this scope's procedural
  // scopes terminates (LRM 9.6.2). A scope of the design hierarchy is
  // replicated with its instance, so the cell is a field of this scope's
  // object and a referrer standing on this object is already standing on it.
  [[nodiscard]] auto DisableTargetField(hir::ProceduralScopeId scope) const
      -> mir::ClassFieldTarget {
    const std::optional<StaticStorageHome>& home =
        scopes_.Get(scope).disable_target;
    if (!home.has_value()) {
      throw InternalError(
          "StructuralScopeLowerer::DisableTargetField: the scope owns no "
          "disable target, so the source named it nothing and no name could "
          "have reached it -- please report this as a bug");
    }
    return std::get<InstanceFieldHome>(*home).field;
  }

  // The call target a structural subroutine reference resolves to: the class
  // that owns the callable, `hops` enclosing edges out from this one, and the
  // callable's identity within it.
  [[nodiscard]] auto TranslateStructuralSubroutine(
      hir::StructuralHops hops, std::span<const hir::OwnedChildStep> descent,
      hir::StructuralSubroutineId hir_id) const -> mir::Direct {
    const StructuralScopeLowerer& owner = ScopeAt(hops, descent);
    return mir::Direct{
        .target = mir::CallableTarget{
            .owner = owner.class_id_,
            .slot = owner.declared_subroutines_.Get(hir_id).callable}};
  }

  // The MIR field one instance member became. A declaration is one field
  // whatever its multiplicity: what it holds is a handle for a single instance
  // and a sequence of them for an array, which is what the field's type states.
  [[nodiscard]] auto InstanceMemberField(hir::InstanceMemberId hir_id) const
      -> mir::ClassFieldTarget {
    return instance_member_fields_.Get(hir_id);
  }

 private:
  UnitLowerer* owner_;
  const StructuralScopeLowerer* parent_;
  const hir::StructuralScope* hir_scope_;
  // Non-empty only on the design root's own scope, the sole scope whose
  // elaboration spans the whole design. Every source unit's scope and every
  // nested scope leaves it empty.
  DesignNamespaces namespaces_;
  base::Translation<hir::StructuralDataObjectId, mir::ClassFieldTarget>
      data_object_fields_;
  base::Translation<hir::SampledHistoryId, mir::FieldId>
      sampled_history_fields_;
  base::Translation<hir::ConcurrentAssertionId, mir::FieldId>
      concurrent_assertion_fields_;
  base::Translation<hir::InterfacePortId, mir::ClassFieldTarget>
      interface_port_fields_;
  base::Translation<hir::RoutedValueRefId, RouteReach> value_reaches_;
  base::Translation<hir::RoutedObjectRefId, RouteReach> object_reaches_;
  base::Translation<hir::RoutedDisableTargetRefId, RouteReach>
      disable_target_reaches_;
  base::Translation<hir::GenerateId, GenerateBinding> generate_bindings_;
  base::Translation<hir::InstanceMemberId, mir::ClassFieldTarget>
      instance_member_fields_;
  DeclaredScopes scopes_;
  base::Translation<hir::StructuralSubroutineId, DeclaredCallable>
      declared_subroutines_;
  // A process is anonymous (LRM 9.2), so nothing can name it early and it takes
  // no callable identity: its answer is storage alone, where a subroutine's is
  // a whole declared callable.
  base::Translation<hir::ProcessId, StaticVarBindings> process_static_bindings_;
  mir::ClassId class_id_{};
  // Each is a constructor parameter after the prefix every scope takes, and is
  // filled from it before anything the construction does can read it.
  std::vector<ConstructionValue> construction_values_;
  // What the unit published of the object this scope is -- an instance of the
  // unit, or a generate block inside one: what it published as its first
  // fields, in the order the signature states them, and a method per published
  // subroutine. The class above extends it with everything the lowering adds. A
  // referrer compiles against this one and holds nothing else, so what the unit
  // adds while lowering its bodies moves no field a referrer reads.
  mir::ClassId published_class_id_{};
  // The subroutines this scope published, in the order its signature states
  // them. Settled while the shape is declared, where a subroutine's identity
  // is taken; read where the published class is built.
  std::vector<PublishedSubroutine> published_subroutines_;
  std::vector<std::unique_ptr<StructuralScopeLowerer>> children_;
  // The classes this scope declares (LRM 23.9). A class declared here is a type
  // of this scope's instance (LRM 6.22), so the scope both settles its shape
  // and lowers its bodies -- which is what gives a class body the reach a
  // process of the scope has, and the instance to record.
  std::vector<ClassDeclLowerer> class_lowerers_;
};

// Which property a class property access names (LRM 8.4): the member at the
// position the class declaring it gave it, this unit's or the one another
// unit published.
template <typename Lowerer>
auto PropertyNameOf(Lowerer& lowerer, const hir::ClassPropertyTarget& target)
    -> PropertyName {
  return std::visit(
      Overloaded{
          [&](const hir::LocalClassPropertyTarget& local) -> PropertyName {
            return lowerer.Owner().TranslateClassPropertyTarget(local);
          },
          [&](const hir::ExternalClassPropertyTarget& published)
              -> PropertyName {
            return lowerer.Owner().MakeCrossUnitClassFieldTarget(published);
          }},
      target);
}

// The storage one class property access reaches, over `receiver` (LRM 8.4).
template <typename Lowerer>
auto BuildClassPropertyAccess(
    Lowerer& lowerer, const WalkFrame& frame, mir::ExprId receiver,
    const hir::ClassPropertyTarget& target, mir::TypeId reached) -> mir::Expr {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  return PropertyStorage(
      unit, *frame.current_block, ReportedObject(unit, frame, receiver),
      PropertyNameOf(lowerer, target), reached);
}

// The type `field` was declared with, read off its class's shape, which is
// settled before any body lowers.
[[nodiscard]] inline auto FieldTypeOf(
    const UnitLowerer& unit_lowerer, const mir::ClassFieldTarget& field)
    -> mir::TypeId {
  return unit_lowerer.GetClassShape(field.owner).fields.Get(field.slot).type;
}

// A value the walk has reached, and the type it has there.
struct ReachedObject {
  mir::ExprId expr;
  mir::TypeId type;
};

// Picks one object out of a value standing for several: one index per instance
// select (LRM 23.6), each taking a dimension off what the value holds. A select
// is resolved where the name is, so an index crosses as a constant rather than
// as a value the design computes, and a value standing for one object takes no
// select and comes back as it went in.
auto ApplyInstanceSelects(
    UnitLowerer& unit_lowerer, mir::Block& block, ReachedObject reached,
    std::span<const std::uint32_t> selects) -> ReachedObject;

// From `object`, a pointer to the object of `scope`, down the owned children
// `descent` names: each element the same descent a route makes, so the object
// reached is the one a route naming those elements would reach.
auto DescendOwnedChildren(
    const StructuralScopeLowerer& scope, mir::Block& block, mir::ExprId object,
    std::span<const hir::OwnedChildStep> descent) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
