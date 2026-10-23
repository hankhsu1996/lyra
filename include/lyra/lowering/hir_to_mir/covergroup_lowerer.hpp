#pragma once

#include <cstdint>
#include <optional>
#include <span>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/coverage_library.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/declared_class.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/external_class.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

class ProcessLowerer;
class StructuralScopeLowerer;
class UnitLowerer;

// The class a covergroup is written as (LRM 19.3), as a lowering reaches it
// wherever the covergroup is declared: what the covergroup's unit published of
// it, and where each property of the class sits.
struct CovergroupClass {
  hir::CovergroupRef covergroup;
  const hir::PublishedCovergroup* published;
  CovergroupLayout layout;
};

[[nodiscard]] auto CovergroupClassOf(
    const UnitLowerer& unit_lowerer, const hir::CovergroupRef& covergroup)
    -> CovergroupClass;

// The class itself, as a construction or a base names it.
[[nodiscard]] auto CovergroupClassRef(
    UnitLowerer& unit_lowerer, const hir::CovergroupRef& covergroup)
    -> mir::DeclaredClassRef;

// The property at `index` of the class, as a member access names it, and the
// type it holds.
[[nodiscard]] auto CovergroupPropertyTarget(
    UnitLowerer& unit_lowerer, const CovergroupClass& cls, std::uint32_t index)
    -> mir::ClassFieldTarget;
[[nodiscard]] auto CovergroupPropertyType(
    UnitLowerer& unit_lowerer, const CovergroupClass& cls, std::uint32_t index)
    -> mir::TypeId;

// The record this unit keeps of the class another unit writes for its
// covergroup, laid out from what that unit published.
[[nodiscard]] auto PublishedCovergroupClass(
    UnitLowerer& unit_lowerer, const hir::ExternalCovergroup& published)
    -> mir::ExternalClass;

// Writes the class one covergroup this unit declares is: its properties, its
// construction over the coverage library, `sample`, the record of its type,
// and the execution sampling it at its event (LRM 19.3, 19.8).
//
// A covergroup a structural scope declares is a type of that scope's instance
// (LRM 6.22), so the object records the instance and the covergroup's own
// expressions reach that scope's declarations as a body of the scope does;
// one a class embeds reaches the object embedding it (LRM 19.4). The lowering
// runs in the two stages a class's does: the shape, which a peer reads while
// its own bodies lower, and then the bodies.
class CovergroupLowerer {
 public:
  CovergroupLowerer(
      UnitLowerer& unit_lowerer, hir::CovergroupId covergroup,
      const StructuralScopeLowerer* declaring_scope);

  // Settles the class's shape. `declaring_shape` is the not-yet-published
  // shape of the scope declaring the covergroup, null for a namespace unit's.
  // `type_event` is where that scope keeps the event its blocks trigger for
  // the covergroup's type, for one sampled at the blocks of an instance; one
  // a namespace unit declares keeps it itself.
  void DeclareShape(
      ClassShape* declaring_shape, std::optional<StaticStorageHome> type_event);

  // Adds to `triggers` what each block the covergroup is sampled at owes the
  // event kept for its type, for one sampled at blocks no object embedding it
  // owns (LRM 19.3).
  void AddTypeBlockEvents(BlockEventTriggers& triggers) const;

  // Writes every body of the class and commits it to the unit, bringing up
  // what the class keeps for its type where `bring_up` brings up the rest of
  // what replicates the covergroup's declaration.
  auto PopulateBodies(
      const WalkFrame& declaring_frame, const StorageBringUp& bring_up)
      -> diag::Result<void>;

 private:
  UnitLowerer* unit_lowerer_;
  hir::CovergroupId covergroup_;
  const hir::CovergroupDecl* decl_;
  const StructuralScopeLowerer* declaring_scope_;
  mir::ClassId class_id_;
  std::optional<StaticStorageHome> type_event_;
  DeclaredScopes scopes_;
};

// The dispatch role of the method a class embedding `covergroup` makes it by
// (LRM 19.4.1): introduced by the class embedding the first covergroup of a
// lineage, overridden by each class embedding one extending it.
[[nodiscard]] auto CovergroupFactoryDispatch(
    UnitLowerer& unit_lowerer, hir::CovergroupId covergroup)
    -> mir::VirtualDispatchRole;

// The body of that method, of the class whose shape is `shape`: it makes the
// covergroup's instance with the object as the embedding one and the arguments
// it is handed, and answers it as the first covergroup of the lineage, whose
// variable every class of the lineage names (LRM 19.4.1).
[[nodiscard]] auto LowerCovergroupFactory(
    UnitLowerer& unit_lowerer, const WalkFrame& owner, const ClassShape& shape,
    hir::CovergroupId covergroup) -> diag::Result<mir::CallableCode>;

// The triggers each block of a class's methods owes the covergroups the class
// embeds that are sampled at it (LRM 19.3), added to `triggers`: the event of
// the record of each instance a property of the object holds.
void AddEmbeddedBlockEvents(
    UnitLowerer& unit_lowerer, hir::ClassId embedding, const ClassShape& shape,
    BlockEventTriggers& triggers);

// LRM 19.3 `new` of a covergroup: the first covergroup of a lineage a class
// embeds made by the embedding object's method, which a class extending the
// embedding one overrides (LRM 19.4.1), and any other by its class's
// construction.
template <ExprLowerer Lowerer>
auto LowerCovergroupNewExpr(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::CovergroupNewExpr& created, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

// A covergroup's built-in method (LRM 19.8): `sample` of the instance, and the
// rest the coverage library's, of the instance's record, of an item's library
// object, or of the type's record.
template <ExprLowerer Lowerer>
auto LowerCovergroupMethodCallExpr(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::CovergroupMethodCallExpr& call) -> diag::Result<mir::Expr>;

// The member an `option` or `type_option` names (LRM 19.7, 19.10): a field of
// the options the coverage library keeps for the instance, the item or the
// type, read and written in place.
template <ExprLowerer Lowerer>
auto CovergroupOptionPath(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::CovergroupOptionExpr& option) -> diag::Result<AccessPath>;

// A coverage database function (LRM 19.9): the coverage library's function of
// the same name.
template <ExprLowerer Lowerer>
auto LowerCoverageDatabaseCall(
    Lowerer& lowerer, const WalkFrame& frame,
    support::CoverageDatabaseCall database, const hir::CallExpr& call)
    -> diag::Result<mir::Expr>;

// Triggers `triggers`, appending to the block `frame` is writing.
void AppendBlockEventTriggers(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    std::span<const BlockEventTrigger> triggers);

}  // namespace lyra::lowering::hir_to_mir
