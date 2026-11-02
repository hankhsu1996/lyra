#pragma once

#include <cstdint>
#include <optional>
#include <variant>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
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
// `member_type`.
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

// What that member is, which is a separate question from where it is: the
// cell itself, or a reference bound to it (LRM 23.3.3.2) -- a capability
// wrapper and not an address, so what reaches the cell through it comes from
// its type rather than from pointer arithmetic.
enum class MemberKind : std::uint8_t {
  kCell,
  kReference,
};

// The runtime endpoint a reader-relative reference reaches: where its member
// is and what the member is. One endpoint serves value read, value write, and
// change observation.
struct BoundEndpoint {
  MemberPlace place;
  MemberKind kind = MemberKind::kCell;
};

// The reference `expr` is where it names the whole of one declaration a route
// reaches, as its own name or as a name a view defines for it. What a
// connection, a drive or a constraint does with such a target depends on the
// declaration, and a part of one or a join of several is no such target.
[[nodiscard]] auto WholeRoutedTarget(
    const UnitLowerer& owner, const hir::StructuralScope& scope,
    const hir::Expr& expr) -> std::optional<hir::RoutedValueRef>;

// Resolves a value reference to its endpoint. Pure: it reads identities and
// types and mutates no block.
[[nodiscard]] auto BindEndpoint(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RoutedValueRef& reference) -> BoundEndpoint;

// The observable cell as an lvalue expression -- the surface every operation on
// the cell acts through, a read, a write and a sampled read alike. A slot is
// opened to reach its member; a member that is the cell, and a reference that
// answers for one, are named directly. Appends any sub-expressions to
// `frame.current_block` and returns the top expression unadded, the same
// contract as a structural field access.
[[nodiscard]] auto EndpointCellExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::Expr;

// A borrowed pointer to what a wait on the endpoint enrols on. A slot pointing
// at the cell hands it over; a reference answers with what it reports to, since
// a wait enrols on whatever a write through it tells; anything else takes the
// address of the cell it names. Appends to `block` and returns the pointer's
// id.
[[nodiscard]] auto EndpointObservablePtr(
    mir::Builder block, const WalkFrame& frame,
    const mir::CompilationUnit& unit, const BoundEndpoint& endpoint)
    -> mir::ExprId;

// What a wait on the storage `reference` names enrols on (LRM 13.5.2):
// whatever a write through the reference is told to, the variable or the
// object a property belongs to, as an erased pointer -- null where the storage
// belongs to nothing. Appends to `block` and returns the pointer's id.
[[nodiscard]] auto ReferenceReportsTo(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId reference)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
