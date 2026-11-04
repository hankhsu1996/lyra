#pragma once

#include <optional>
#include <variant>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

// The member a reference ends at, reached from the reader: `field` of the
// object `hops` enclosing edges out -- the reader's own at zero -- or the
// member a stored route's `slot` points at. Either way the member's type is
// `member_type`, which says what the member is: a variable's cell, a net, or
// a reference bound to what its connection names (LRM 23.3.3.2). One member
// serves value read, value write, and change observation.
struct MemberAtHops {
  mir::EnclosingHops hops;
  mir::ClassFieldTarget field;
  mir::TypeId member_type;
};
struct MemberThroughSlot {
  mir::FieldId slot;
  mir::TypeId member_type;
};
using MemberPlace = std::variant<MemberAtHops, MemberThroughSlot>;

// The reference `expr` is where it names the whole of one declaration a route
// reaches, as its own name or as a name a view defines for it. What a
// connection, a drive or a constraint does with such a target depends on the
// declaration, and a part of one or a join of several is no such target.
[[nodiscard]] auto WholeRoutedTarget(
    const UnitLowerer& owner, const hir::StructuralScope& scope,
    const hir::Expr& expr) -> std::optional<hir::RoutedValueRef>;

// Resolves a value reference to the member it ends at. Pure: it reads
// identities and types and mutates no block.
[[nodiscard]] auto BindEndpoint(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RoutedValueRef& reference) -> MemberPlace;

// The observable cell as an lvalue expression -- the surface every operation on
// the cell acts through, a read, a write and a sampled read alike. A slot is
// opened to reach its member; a member that is the cell, and a reference that
// answers for one, are named directly. Appends any sub-expressions to
// `frame.current_block` and returns the top expression unadded, the same
// contract as a structural field access.
[[nodiscard]] auto EndpointCellExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const MemberPlace& endpoint) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
