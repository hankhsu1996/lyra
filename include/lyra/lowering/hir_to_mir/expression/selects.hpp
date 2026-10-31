#pragma once

// LRM 11.5 select expressions: element-, range-, and member-select.
// LRM 7.2.1: a packed struct / union field access lowers as a slice over
// the aggregate's bit plane -- MIR carries no struct-specific node.

#include <cstdint>
#include <functional>
#include <optional>

#include "lyra/base/component_index.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The step `receiver[idx]` takes (LRM 7.4.5 / 7.5 / 7.8 / 7.10 / 11.5.1), with
// `idx` written in the coordinates the receiver was declared with. A packed
// value is reached by the bits one element of its outermost dimension
// occupies, an associative array by the key itself, and every other array by
// the element's position. Every site that reaches an element states the step
// the same way, whether the source wrote a select or an assignment pattern
// named the element by key, so what a container needs is answered from its
// type in one place.
[[nodiscard]] auto ElementStep(
    UnitLowerer& unit_lowerer, mir::Builder block, hir::TypeId receiver_type,
    mir::ExprId idx_id, mir::TypeId part_type) -> DescentStep;

// The step `aggregate.member` takes (LRM 7.2, 7.3). An unpacked aggregate's
// member is named by its declaration-order position: a structure's is a
// component, storage of its own, and a union's lies at the union's own start,
// with no identity of its own. A packed aggregate's member is bits of the one
// vector the aggregate is.
[[nodiscard]] auto MemberStep(
    UnitLowerer& unit_lowerer, mir::Builder block, const hir::Type& aggregate,
    base::ComponentIndex index, mir::TypeId part_type) -> DescentStep;

// LRM 11.4.14.4: the elements a `with` names of an array of `array_type`:
// where the first of them lies in the array's own numbering, and how many
// there are, as a machine count, each evaluated once however often it is read.
// `frame` is where the range is evaluated, as a select of the array, so a `$`
// in it names a queue's last element.
struct StreamWindow {
  mir::ExprId start;
  mir::ExprId count;
};
template <ExprLowerer Lowerer>
[[nodiscard]] auto LowerStreamWindow(
    Lowerer& lowerer, const WalkFrame& frame, hir::TypeId array_type,
    const hir::StreamRange& range) -> diag::Result<StreamWindow>;

// The stream of the elements `operand`'s `with` names of the array it streams,
// as a run of bits, an element outside the array streaming as the element
// default (LRM 11.4.14.4). The array and its range are evaluated once.
template <ExprLowerer Lowerer>
[[nodiscard]] auto LowerStreamWindowRun(
    Lowerer& lowerer, const WalkFrame& frame, const hir::StreamOperand& operand)
    -> diag::Result<mir::Expr>;

// `arr[i]` read as a value: the read the element step takes from `base_id`, a
// value of `base_type`.
[[nodiscard]] auto BuildElementAccessCallExpr(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId base_id,
    hir::TypeId base_type, mir::ExprId idx_id, mir::TypeId result_type)
    -> mir::Expr;

// Whether `subject`, a tagged union of type `subject_type`, holds the member at
// `index`, as a machine boolean, its tag compared as `keyword` compares; a
// packed union of one member carries a tag of no bits (LRM 7.3.2) and always
// holds it. What a pattern tests (LRM 12.6), under the keyword of the case
// statement governing it, and what `%p` prints (LRM 21.2.1.6), exactly, both
// ask it.
[[nodiscard]] auto BuildHoldsMember(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId subject,
    hir::TypeId subject_type, base::ComponentIndex index,
    hir::CaseKeyword keyword) -> mir::ExprId;

// The two below read bits of a packed value's vector by position, for a
// consumer that has no source-level select to lower: pattern matching (LRM
// 12.6) destructures a value the source named only as a whole.

// The bits starting at `bit_offset`, as many as `result_type` is wide, as a
// value of that type. Nothing is checked: the caller states which bits it
// wants.
[[nodiscard]] auto BuildPackedBitsRead(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId base,
    std::uint64_t bit_offset, mir::TypeId result_type) -> mir::Expr;

// Member `index` of the aggregate `projection` describes, at `result_type`.
// Nothing is checked: a caller reaching a member of a tagged union this way
// has already tested the tag names it (LRM 11.9, 12.6).
[[nodiscard]] auto BuildPackedMemberRead(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId base,
    const PackedProjection& projection, base::ComponentIndex index,
    mir::TypeId result_type) -> mir::Expr;

// A select's meaning is independent of the enclosing scope, so one template
// over the pass class serves both the procedural and structural contexts;
// explicit instantiations live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerHirElementSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirClassPropertyAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ClassPropertyAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// A read through a member a tag guards (LRM 11.9), where the union lies in
// storage and is read as it is now, is one access: its whole path is taken,
// every tag it passes checked, and the part it selects read and kept, all in
// one block of steps, so nothing the enclosing expression evaluates -- a
// sibling operand retagging the union -- runs between a check and the read it
// guards. A borrow reached this way takes the value kept, since a member is
// never a reference target (LRM 13.5.2) and the borrow outlives the steps; a
// call that only reads the object is entered on it among the steps instead.
template <ExprLowerer Lowerer>
auto IsTagCheckedRead(
    Lowerer& lowerer, const WalkFrame& frame, const hir::Expr& select) -> bool;
template <ExprLowerer Lowerer>
auto LowerTagCheckedRead(
    Lowerer& lowerer, WalkFrame frame, const hir::Expr& select)
    -> diag::Result<mir::Expr>;

// A use of what such a select reaches, made in the same way: the select's
// whole path is taken in one block of steps and handed to the use, named
// there, and the value the use answers is what the steps end with. Each place
// the use forms on the path checks every tag it passes (LRM 11.9), so a use
// evaluating what else it needs before forming one runs nothing between a
// check and the access the check guards.
using TagCheckedUse = std::function<diag::Result<mir::ExprId>(
    BlockBuilder& steps, const AccessPath& path)>;
template <ExprLowerer Lowerer>
auto LowerTagCheckedUse(
    Lowerer& lowerer, WalkFrame frame, const hir::Expr& select,
    const TagCheckedUse& use) -> diag::Result<mir::Expr>;

// A select named as a part rather than read: the path its base names, one step
// deeper. Each adds the same step the read of it takes, so what comes back is
// rooted at the cell itself rather than at the storage it stands for, and is
// the one statement of the part every use of it is given. A select taken from
// a queue has what its base computes evaluated where it is reached, once: `$`
// in its index or bounds reads that queue again (LRM 7.10.1).
template <ExprLowerer Lowerer>
auto LowerHirElementSelectExprPath(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<AccessPath>;
template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExprPath(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<AccessPath>;
// The indices of its array a slice holds, at its left and at its right, as
// machine integers.
struct SliceIndices {
  mir::ExprId left;
  mir::ExprId right;
};

// The memory a task loads or dumps by its indices (LRM 21.3.4.4, 21.4, 21.5):
// the part it is, and, where the source names a slice of a fixed-size or
// dynamic array, the indices of that array the slice holds, which are the
// addresses the task reaches it by. Any other memory is addressed by its
// type's ranges.
struct NamedMemory {
  AccessPath path;
  std::optional<SliceIndices> slice;
};

// A slice of fixed size, taken of a fixed-size or a dynamic array, named as
// such a memory. The slice's type numbers its elements from zero, since its
// range may follow the instance or the run, so the indices are the select's
// own; the index the slice starts at is evaluated once for the part and both
// indices.
template <ExprLowerer Lowerer>
auto LowerHirArraySliceWithIndices(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<NamedMemory>;
template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExprPath(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<AccessPath>;
// A property of the object `receiver` reaches, as the target of a write: the
// property's own place, lying in that object, which the write is opened on.
template <ExprLowerer Lowerer>
auto PropertyPath(
    Lowerer& lowerer, const WalkFrame& frame, mir::ExprId receiver,
    const hir::ClassPropertyTarget& target, mir::TypeId result_type)
    -> AccessPath;
template <ExprLowerer Lowerer>
auto LowerHirClassPropertyAccessExprPath(
    Lowerer& lowerer, WalkFrame frame, const hir::ClassPropertyAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<AccessPath>;

// A member of the interface instance a virtual interface holds (LRM 25.9), as
// the member's own storage, in either context: a read takes the value it holds
// and a write lands in it, the way a member reached through a port is.
template <ExprLowerer Lowerer>
auto LowerHirInterfaceMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::InterfaceMemberAccessExpr& sel) -> diag::Result<mir::Expr>;

// An interface instance declared inside the one a virtual interface holds, as
// the value a virtual interface of its own type would hold (LRM 25.9).
template <ExprLowerer Lowerer>
auto LowerHirInterfaceInstanceAccessExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::InterfaceInstanceAccessExpr& sel, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
