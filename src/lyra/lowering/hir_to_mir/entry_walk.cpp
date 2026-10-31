#include "lyra/lowering/hir_to_mir/entry_walk.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builder.hpp"
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
    const mir::CompilationUnit& unit, mir::Builder block,
    const std::optional<hir::UnpackedRange>& declared, mir::ExprId position)
    -> mir::ExprId {
  if (!declared.has_value()) {
    return position;
  }
  return block.Add(
      mir::MakeBinary(
          unit, block.Body(),
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
  mir::Builder block = frame.Builder();
  const mir::TypeId int_type = unit.builtins.int_type;
  const mir::ExprId count = EntryCount(unit, block, array);
  const mir::LocalId position = frame.bindings->DeclareAnonymous(int_type);

  mir::Block body;
  mir::Builder body_builder = block.Into(body);
  BreakTarget walk(*frame.bindings);
  const WalkFrame at = frame.WithBlock(&body).WithBreakLeaving(walk);
  const mir::LocalId index = IndexLocal(at, index_origin, int_type);
  body_builder.AppendStmt(
      mir::LocalDeclStmt{
          .target = index,
          .init = IndexAtPosition(
              unit, body_builder, array.declared,
              body_builder.ReadLocal(position, int_type))});
  const EntryPlace place{
      .index = index, .index_type = int_type, .position = position};
  AccessPath named = NamedIn(array.path, body_builder);
  DescentStep visited = StepToVisited(unit, body_builder, place, element_type);
  auto ran = visit(
      at, VisitedEntry{
              .element = PathValue(
                  unit, body_builder,
                  DescendInto(std::move(named), std::move(visited))),
              .place = place});
  if (!ran) return std::unexpected(std::move(ran.error()));

  const mir::BlockId body_scope = block.AddScope(std::move(body));
  return BuildPositionWalkStmt(
      unit, frame, block, count, position, order, body_scope, walk.Label());
}

// The loop over an associative array, by its keys, which the index variable
// holds in turn.
auto BuildKeyedLoopStmt(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const WalkedArray& array,
    mir::AssociativeArrayType keyed, WalkOrder order,
    std::optional<BindingOriginId> index_origin, const EntryVisit& visit)
    -> diag::Result<mir::Stmt> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder block = frame.Builder();
  const mir::TypeId key_type = keyed.key_type;

  const mir::LocalId key = IndexLocal(frame, index_origin, key_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = key,
          .init = block.Add(BuildDefaultValueExpr(unit, block, key_type))});

  mir::Block body;
  BreakTarget walk(*frame.bindings);
  const WalkFrame at = frame.WithBlock(&body).WithBreakLeaving(walk);
  mir::Builder body_builder = at.Builder();
  const EntryPlace place{
      .index = key, .index_type = key_type, .position = std::nullopt};
  AccessPath named = NamedIn(array.path, body_builder);
  DescentStep visited =
      StepToVisited(unit, body_builder, place, keyed.element_type);
  auto ran = visit(
      at, VisitedEntry{
              .element = PathValue(
                  unit, body_builder,
                  DescendInto(std::move(named), std::move(visited))),
              .place = place});
  if (!ran) return std::unexpected(std::move(ran.error()));
  const mir::BlockId body_scope = block.AddScope(std::move(body));

  const auto array_value = [&](const WalkFrame& in) {
    mir::Builder steps = in.Builder();
    return PathValue(unit, steps, NamedIn(array.path, steps));
  };
  const auto key_place = [&](const WalkFrame& in) {
    return AccessPath{
        .owner = in.Builder().LocalPlace(key, key_type), .descent = {}};
  };
  return BuildKeyLoopStmt(
      unit_lowerer, frame, array_value, key_place, key_type, order, body_scope,
      walk.Label());
}

}  // namespace

auto StepToVisited(
    mir::CompilationUnit& unit, mir::Builder block, const EntryPlace& place,
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
      PathValueType(unit_lowerer.Unit(), frame.Builder().Body(), path);
  return WalkedArray{
      .path = SettledForRead(unit_lowerer, frame, std::move(path)),
      .type = type,
      .declared = declared};
}

auto WalkedArrayOf(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId object,
    std::optional<hir::UnpackedRange> declared) -> WalkedArray {
  mir::Builder block = frame.Builder();
  const mir::ExprId lying =
      mir::ReadIfOverlaid(unit_lowerer.Unit(), block, object);
  const mir::TypeId type = block.Get(lying).type;
  const mir::ExprId owner =
      mir::IsPlace(block.Get(lying).data)
          ? lying
          : block.LocalPlace(DeclareLocal(frame, lying), type);
  return WalkedArrayAt(
      unit_lowerer, frame, AccessPath{.owner = owner, .descent = {}}, declared);
}

auto EntryCount(
    mir::CompilationUnit& unit, mir::Builder block, const WalkedArray& array)
    -> mir::ExprId {
  const auto* fixed = unit.types.Get(array.type).As<mir::UnpackedArrayType>();
  if (fixed != nullptr) {
    const auto size = static_cast<std::int64_t>(fixed->Size());
    return mir::BuildIntLiteral(unit, block, size);
  }
  return block.Add(
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
