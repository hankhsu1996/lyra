#pragma once

#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/callable_storage_plan.hpp"
#include "lyra/lowering/hir_to_mir/package_initialization.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::lowering::hir_to_mir {

struct ChildStructuralScopeBinding {
  mir::ClassId scope_id;
  // The child's SV-visible label: its by-name lookup key in the runtime tree,
  // and the runtime_label passed to its construction.
  std::string label;
  // The borrowed typed handle on the parent's class, present for a scalar
  // generate child (a typed segment navigates through it); absent for a
  // generate-for array element, which the route reaches by indexed GetChild.
  std::optional<mir::FieldId> companion;
};

// How a hierarchical route reaches an owned-child head. `label` is the by-name
// key. `companion` (scalar layout-visible children) is the parent's borrowed
// typed handle the route projects for a scalar segment. `target` is the child's
// intra-unit class, present when the artifact owns the child's body (so the
// receiver stays typed / the indexed fallback downcasts), absent when the child
// is another compilation unit (opaque from there).
struct OwnedChildAnchor {
  std::string label;
  std::optional<mir::FieldId> companion;
  std::optional<mir::ClassId> target;
};

// The owned-child-scope bindings for a single `hir::Generate`, indexed by
// `hir::StructuralScopeId.value`. A transient table threaded through generate
// installation during constructor lowering, not stored on the pass object.
struct GenerateBindings {
  std::vector<ChildStructuralScopeBinding> by_scope_id;
};

// The MIR slot a HIR routed ref resolves to: the slot's field id bundled
// with the slot's MIR type, so a body reader decides whether the read
// dereferences without re-touching the owning scope.
struct RoutedRefMeta {
  mir::FieldId target = {};
  mir::TypeId slot_type = {};
};

// Lowers one HIR structural scope into one MIR class across two ordered
// phases. Carries the scope-local HIR-to-MIR identity mappings populated in
// `DeclareShape` and consulted in `PopulateBodies`; borrows the enclosing
// scope's lowerer through `parent_` for hops-walked cross-scope lookups; and
// owns descendant scope lowerers in `children_` so the structural-scope tree
// stays alive across the phase boundary. The lowerer itself holds no
// pointer to the in-progress `mir::Class`: body handlers reach it through
// `frame.current_class`.
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
  // data` routing each kind to the per-family handler in `expression/*.cpp`.
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

  // Resolve a DPI-C import reference to its HIR declaration by walking `hops`
  // scopes outward. The import's task-or-function kind decides whether its call
  // site is a suspension point (LRM 35.8), which the statement lowering reads
  // from here.
  [[nodiscard]] auto LookupForeignImport(
      hir::StructuralHops hops, hir::ForeignImportId id) const
      -> const hir::ForeignImportDecl& {
    if (hops.value == 0) {
      return hir_scope_->foreign_imports.Get(id);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::LookupForeignImport: hops walk ran "
          "past the root scope");
    }
    return parent_->LookupForeignImport(
        hir::StructuralHops{.value = hops.value - 1}, id);
  }

  void AddRoutedRefTarget(mir::FieldId target, mir::TypeId slot_type) {
    routed_ref_targets_.push_back(
        RoutedRefMeta{.target = target, .slot_type = slot_type});
  }

  [[nodiscard]] auto RoutedRefTarget(hir::RoutedRefId hir_id) const
      -> const RoutedRefMeta& {
    return routed_ref_targets_.at(hir_id.value);
  }

  // The declared shape of the scope `hops` enclosing edges out from this one,
  // in the same compilation unit. An enclosing routed reference reaches its
  // leaf as a typed member of this shape, the same by-name typed reach a
  // sibling or child route uses within the unit.
  [[nodiscard]] auto EnclosingClassShapeAtHops(hir::StructuralHops hops) const
      -> const mir::ClassShape& {
    if (hops.value == 0) {
      return owner_->GetClassShape(class_id_);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::EnclosingClassShapeAtHops: hops walk ran "
          "past the root scope");
    }
    return parent_->EnclosingClassShapeAtHops(
        hir::StructuralHops{.value = hops.value - 1});
  }

  // Registry identity of the class returned by `EnclosingClassShapeAtHops`;
  // carried alongside so an owner-qualified field or method target names the
  // arena without a reverse lookup from the shape.
  [[nodiscard]] auto EnclosingClassIdAtHops(hir::StructuralHops hops) const
      -> mir::ClassId {
    if (hops.value == 0) {
      return class_id_;
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::EnclosingClassIdAtHops: hops walk ran "
          "past the root scope");
    }
    return parent_->EnclosingClassIdAtHops(
        hir::StructuralHops{.value = hops.value - 1});
  }

  void MapStructuralDataObject(
      hir::StructuralDataObjectId hir_id, mir::FieldId mir_id) {
    if (hir_id.value != structural_data_object_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::MapStructuralDataObject: HIR structural "
          "data objects must be mapped in HIR id order");
    }
    structural_data_object_map_.push_back(mir_id);
  }

  [[nodiscard]] auto TranslateStructuralDataObject(
      hir::StructuralHops hops, hir::StructuralDataObjectId hir_id) const
      -> mir::FieldId {
    return LookupStructuralDataObjectAtHops(hops, hir_id);
  }

  void MapGenerate(hir::GenerateId hir_id, GenerateBindings bindings) {
    if (hir_id.value != generate_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::MapGenerate: HIR generates must be mapped "
          "in HIR id "
          "order");
    }
    generate_map_.push_back(std::move(bindings));
  }

  [[nodiscard]] auto LookupGenerateBindings(hir::GenerateId hir_id) const
      -> const GenerateBindings& {
    if (hir_id.value >= generate_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::LookupGenerateBindings: unmapped HIR "
          "generate");
    }
    return generate_map_[hir_id.value];
  }

  // Resolves an owned-child reference (the `child` field of a
  // `hir::DownwardHead`) to how the route reaches it: its label, the parent's
  // borrowed companion handle when scalar, and its intra-unit target class when
  // the artifact owns the child's body. A named-block head carries only its SV
  // label; its companion and target class are recovered from the materialized
  // procedural scope the label names. `hops == 0` reads this scope's own
  // tables; `hops > 0` walks the parent chain to an enclosing scope, used by
  // the sibling-of-ancestor install when the head lives outside the referrer's
  // frame.
  [[nodiscard]] auto TranslateOwnedChild(
      hir::StructuralHops hops,
      const std::variant<
          hir::InstanceMemberId, hir::GenerateChildRef, hir::NamedBlockRef>&
          child) const -> OwnedChildAnchor {
    return LookupOwnedChildAnchorAtHops(hops, child);
  }

  void MapStructuralSubroutine(
      hir::StructuralSubroutineId hir_id, mir::CallableId mir_id) {
    if (hir_id.value != structural_subroutine_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::MapStructuralSubroutine: HIR "
          "structural subroutines must be mapped in HIR id order");
    }
    structural_subroutine_map_.push_back(mir_id);
  }

  [[nodiscard]] auto TranslateStructuralSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId hir_id) const
      -> mir::Direct {
    const StructuralSubroutineRef ref =
        LookupStructuralSubroutineAtHops(hops, hir_id);
    return mir::Direct{
        .target = mir::CallableTarget{.owner = ref.owner, .slot = ref.slot}};
  }

  // Records the parent's borrowed companion handle for one instance member, in
  // HIR instance-member id order. A scalar instance carries a companion; an
  // instance array carries none (reached by indexed GetChild).
  void MapInstanceCompanion(
      hir::InstanceMemberId hir_id, std::optional<mir::FieldId> companion) {
    if (hir_id.value != instance_companion_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::MapInstanceCompanion: instance members must "
          "be mapped in HIR id order");
    }
    instance_companion_map_.push_back(companion);
  }

  [[nodiscard]] auto InstanceCompanion(hir::InstanceMemberId hir_id) const
      -> std::optional<mir::FieldId> {
    if (hir_id.value >= instance_companion_map_.size()) return std::nullopt;
    return instance_companion_map_[hir_id.value];
  }

 private:
  [[nodiscard]] auto LookupOwnedChildAnchorAtHops(
      hir::StructuralHops hops,
      const std::variant<
          hir::InstanceMemberId, hir::GenerateChildRef, hir::NamedBlockRef>&
          child) const -> OwnedChildAnchor {
    if (hops.value == 0) {
      return std::visit(
          Overloaded{
              [&](const hir::InstanceMemberId& id) -> OwnedChildAnchor {
                // A module instance's body is another compilation unit, so it
                // has no intra-unit target class; the head member is typed but
                // opaque from there.
                return OwnedChildAnchor{
                    .label = hir_scope_->instance_members.Get(id).instance_name,
                    .companion = id.value < instance_companion_map_.size()
                                     ? instance_companion_map_[id.value]
                                     : std::nullopt,
                    .target = std::nullopt};
              },
              [&](const hir::GenerateChildRef& g) -> OwnedChildAnchor {
                if (g.generate.value >= generate_map_.size()) {
                  throw InternalError(
                      "StructuralScopeLowerer::TranslateOwnedChild: unmapped "
                      "HIR generate");
                }
                const auto& b = generate_map_[g.generate.value].by_scope_id.at(
                    g.scope.value);
                return OwnedChildAnchor{
                    .label = b.label,
                    .companion = b.companion,
                    .target = b.scope_id};
              },
              [&](const hir::NamedBlockRef& nb) -> OwnedChildAnchor {
                const auto& e = scope_materialization_.FindHead(nb.label);
                return OwnedChildAnchor{
                    .label = e.label,
                    .companion = e.companion_field,
                    .target = e.class_id};
              },
          },
          child);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateOwnedChild: hops exceed scope "
          "chain depth");
    }
    return parent_->LookupOwnedChildAnchorAtHops(
        hir::StructuralHops{hops.value - 1}, child);
  }

  [[nodiscard]] auto LookupStructuralDataObjectAtHops(
      hir::StructuralHops hops, hir::StructuralDataObjectId hir_id) const
      -> mir::FieldId {
    if (hops.value == 0) {
      if (hir_id.value >= structural_data_object_map_.size()) {
        throw InternalError(
            "StructuralScopeLowerer::LookupStructuralDataObjectAtHops: "
            "unmapped "
            "HIR structural data object");
      }
      return structural_data_object_map_[hir_id.value];
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateStructuralDataObject: hops out "
          "of scope chain");
    }
    return parent_->LookupStructuralDataObjectAtHops(
        hir::StructuralHops{hops.value - 1}, hir_id);
  }

  // The owner-qualified identity of a structural subroutine reached by
  // `hops` steps up the scope chain: the class that owns the subroutine's
  // callable arena and the slot within that arena.
  struct StructuralSubroutineRef {
    mir::ClassId owner;
    mir::CallableId slot;
  };

  [[nodiscard]] auto LookupStructuralSubroutineAtHops(
      hir::StructuralHops hops, hir::StructuralSubroutineId hir_id) const
      -> StructuralSubroutineRef {
    if (hops.value == 0) {
      if (hir_id.value >= structural_subroutine_map_.size()) {
        throw InternalError(
            "StructuralScopeLowerer::TranslateStructuralSubroutine: "
            "unmapped HIR subroutine");
      }
      return StructuralSubroutineRef{
          .owner = class_id_, .slot = structural_subroutine_map_[hir_id.value]};
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateStructuralSubroutine: hops "
          "exceed scope chain depth");
    }
    return parent_->LookupStructuralSubroutineAtHops(
        hir::StructuralHops{hops.value - 1}, hir_id);
  }

  UnitLowerer* owner_;
  const StructuralScopeLowerer* parent_;
  std::string name_;
  const hir::StructuralScope* hir_scope_;
  // The design root's plan for bringing up package variables in its Initialize
  // phase (LRM 26.2 / 10.5): install every package's cells, then run every
  // package's value initializers in the resolved order. Non-empty only on the
  // design root's own scope -- the sole scope whose elaboration spans the whole
  // design; the lowering only realizes the resolved plan into cross-unit calls
  // and never re-derives it. Every source unit's scope and every nested scope
  // leaves it empty.
  PackageInitializationPlan package_init_plan_;
  std::vector<mir::FieldId> structural_data_object_map_;
  std::vector<mir::CallableId> structural_subroutine_map_;
  std::vector<RoutedRefMeta> routed_ref_targets_;
  std::vector<GenerateBindings> generate_map_;
  // The parent's borrowed companion handle per instance member (nullopt for an
  // instance array). Indexed by `hir::InstanceMemberId`.
  std::vector<std::optional<mir::FieldId>> instance_companion_map_;
  // Per-structural-scope runtime-topology table for materialized procedural
  // storage scopes. Populated during shape declaration; consumed by the
  // per-callable plans and by the runtime-tree construction emitter.
  ProceduralScopeMaterializationTable scope_materialization_;
  // Per-callable storage plan. Outer vector parallels the HIR scope's
  // subroutines / processes arenas; each entry packages the callable's
  // static placements together with a reference to `scope_materialization_`
  // so peer bodies query both through the same plan handle.
  std::vector<CallableStoragePlan> subroutine_storage_plans_;
  std::vector<CallableStoragePlan> process_storage_plans_;
  mir::ClassId class_id_{};
  std::vector<std::unique_ptr<StructuralScopeLowerer>> children_;
};

}  // namespace lyra::lowering::hir_to_mir
