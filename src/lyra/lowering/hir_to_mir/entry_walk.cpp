#include "lyra/lowering/hir_to_mir/entry_walk.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The local holding the index of the entry being visited: one the visit reads
// by `origin` where it is given, and one only the loop reads otherwise.
auto IndexLocal(
    const WalkFrame& frame, std::optional<BindingOriginId> origin,
    mir::TypeId type) -> mir::LocalId {
  return origin.has_value() ? frame.bindings->Declare(*origin, type)
                            : frame.bindings->DeclareAnonymous(type);
}

// The index of the entry at `position` in an array numbered by `declared`:
// the one its declaration gives it for a fixed-size array, which numbers its
// entries from its left bound (LRM 7.4.5, 7.12.4), and the position itself for
// one sized while the program runs, which numbers them from 0 (LRM 7.5, 7.10).
auto IndexAtPosition(
    const mir::CompilationUnit& unit, mir::Block& block,
    const std::optional<hir::UnpackedRange>& declared, mir::ExprId position)
    -> mir::ExprId {
  if (!declared.has_value()) {
    return position;
  }
  return block.exprs.Add(
      mir::MakeBinary(
          unit, block,
          declared->left <= declared->right ? mir::BinaryOp::kAdd
                                            : mir::BinaryOp::kSub,
          mir::BuildIntLiteral(unit, block, declared->left), position,
          unit.builtins.int_type));
}

// The loop over an array that numbers its elements: a count of positions, run
// from the first or from the last.
auto BuildPositionLoopStmt(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const WalkedArray& array,
    mir::TypeId element_type, WalkOrder order,
    std::optional<BindingOriginId> index_origin, const EntryVisit& visit)
    -> diag::Result<mir::Stmt> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId int_type = unit.builtins.int_type;
  const mir::ExprId count = EntryCount(unit, block, array);
  const mir::LocalId position = frame.bindings->DeclareAnonymous(int_type);

  mir::Block body;
  const WalkFrame at = frame.WithBlock(&body);
  const mir::LocalId index = IndexLocal(at, index_origin, int_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = index,
          .init = IndexAtPosition(
              unit, body, array.declared, body.ReadLocal(position, int_type))});
  const EntryPlace place{
      .index = index, .index_type = int_type, .position = position};
  auto ran = visit(
      at, VisitedEntry{
              .element = PathValue(
                  unit, body,
                  DescendInto(
                      NamedIn(array.path, body),
                      StepToVisited(unit, body, place, element_type))),
              .place = place});
  if (!ran) return std::unexpected(std::move(ran.error()));

  const mir::BlockId body_scope = block.child_scopes.Add(std::move(body));
  return BuildPositionWalkStmt(
      unit, frame, block, count, position, order, body_scope);
}

// The loop over an associative array, by its keys, which the index variable
// holds in turn.
auto BuildKeyedLoopStmt(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const WalkedArray& array,
    mir::AssociativeArrayType keyed, WalkOrder order,
    std::optional<BindingOriginId> index_origin, const EntryVisit& visit)
    -> diag::Result<mir::Stmt> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId key_type = keyed.key_type;

  const mir::LocalId key = IndexLocal(frame, index_origin, key_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = key,
          .init =
              block.exprs.Add(BuildDefaultValueExpr(unit, block, key_type))});

  mir::Block body;
  const WalkFrame at = frame.WithBlock(&body);
  const EntryPlace place{
      .index = key, .index_type = key_type, .position = std::nullopt};
  auto ran = visit(
      at, VisitedEntry{
              .element = PathValue(
                  unit, body,
                  DescendInto(
                      NamedIn(array.path, body),
                      StepToVisited(unit, body, place, keyed.element_type))),
              .place = place});
  if (!ran) return std::unexpected(std::move(ran.error()));
  const mir::BlockId body_scope = block.child_scopes.Add(std::move(body));

  return BuildKeyLoopStmt(
      unit_lowerer, frame,
      [&](const WalkFrame& in) {
        mir::Block& steps = *in.current_block;
        return PathValue(unit, steps, NamedIn(array.path, steps));
      },
      [&](const WalkFrame& in) {
        return AccessPath{
            .owner = in.current_block->LocalPlace(key, key_type),
            .descent = {}};
      },
      key_type, order, body_scope, std::nullopt);
}

}  // namespace

auto StepToVisited(
    mir::CompilationUnit& unit, mir::Block& block, const EntryPlace& place,
    mir::TypeId part_type) -> DescentStep {
  if (place.position.has_value()) {
    return DescentStep{
        .to =
            StepToElement{
                .position = BuildOrdinalPosition(
                    unit, block,
                    block.ReadLocal(*place.position, unit.builtins.int_type))},
        .part_type = part_type};
  }
  return DescentStep{
      .to =
          StepToAssocElement{
              .key = block.ReadLocal(place.index, place.index_type)},
      .part_type = part_type};
}

auto DeclaredRangeOf(const UnitLowerer& unit_lowerer, hir::TypeId type)
    -> std::optional<hir::UnpackedRange> {
  const auto* fixed =
      unit_lowerer.Hir().types.Get(type).As<hir::UnpackedArrayType>();
  if (fixed == nullptr) {
    return std::nullopt;
  }
  return fixed->dim;
}

auto WalkedArrayAt(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path,
    std::optional<hir::UnpackedRange> declared) -> WalkedArray {
  const mir::TypeId type =
      PathValueType(unit_lowerer.Unit(), *frame.current_block, path);
  return WalkedArray{
      .path = SettledForRead(unit_lowerer, frame, std::move(path)),
      .type = type,
      .declared = declared};
}

auto WalkedArrayOf(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId object,
    std::optional<hir::UnpackedRange> declared) -> WalkedArray {
  mir::Block& block = *frame.current_block;
  const mir::ExprId lying =
      mir::ReadIfOverlaid(unit_lowerer.Unit(), block, object);
  const mir::TypeId type = block.exprs.Get(lying).type;
  const mir::ExprId owner =
      mir::IsPlace(block.exprs.Get(lying).data)
          ? lying
          : block.LocalPlace(DeclareLocal(frame, lying), type);
  return WalkedArrayAt(
      unit_lowerer, frame, AccessPath{.owner = owner, .descent = {}}, declared);
}

auto EntryCount(
    mir::CompilationUnit& unit, mir::Block& block, const WalkedArray& array)
    -> mir::ExprId {
  const auto* fixed = unit.types.Get(array.type).As<mir::UnpackedArrayType>();
  if (fixed != nullptr) {
    const auto size = static_cast<std::int64_t>(fixed->Size());
    return mir::BuildIntLiteral(unit, block, size);
  }
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kSize,
                          .receiver = PathAsPlace(
                              unit, block, NamedIn(array.path, block))},
                  .arguments = {}},
          .type = unit.builtins.int_type});
}

auto BuildEntryLoopStmt(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const WalkedArray& array,
    WalkOrder order, std::optional<BindingOriginId> index_origin,
    const EntryVisit& visit) -> diag::Result<mir::Stmt> {
  const mir::Type& type = unit_lowerer.Unit().types.Get(array.type);
  if (const auto* keyed = type.As<mir::AssociativeArrayType>()) {
    return BuildKeyedLoopStmt(
        unit_lowerer, frame, array, *keyed, order, index_origin, visit);
  }
  const std::optional<mir::TypeId> element = type.ContainerElementType();
  if (!element.has_value()) {
    throw InternalError(
        "BuildEntryLoopStmt: the array whose entries are visited is no "
        "unpacked array");
  }
  return BuildPositionLoopStmt(
      unit_lowerer, frame, array, *element, order, index_origin, visit);
}

}  // namespace lyra::lowering::hir_to_mir
