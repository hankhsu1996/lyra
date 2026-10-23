#include "lyra/lowering/hir_to_mir/declared_initial_value.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <utility>

#include "lyra/base/component_index.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Whether a member starts as its type does -- its declaration gives it no
// initial value of its own (LRM 7.2.2) -- and that type holds an event a
// declaration gives a new object.
auto MemberTakesNewEvents(
    const hir::TypePool& types, const hir::UnpackedAggregateField& member)
    -> bool {
  return !member.default_init.has_value() &&
         HoldsADeclaredEvent(types, member.type);
}

}  // namespace

// A value of `type` holds such an event as the value itself, as an element of
// a fixed-size array, or as a member of a structure that starts as its type
// does. A variable-size array starts with no elements. An untagged union holds
// no event (LRM 7.3), and a tagged one starts undefined (LRM 11.9), so neither
// starts with one. No other type holds an event.
auto HoldsADeclaredEvent(const hir::TypePool& types, hir::TypeId type) -> bool {
  return types.Get(type).Visit(
      Overloaded{
          [](const hir::EventType&) { return true; },
          [&](const hir::UnpackedArrayType& array) {
            return HoldsADeclaredEvent(types, array.element_type);
          },
          [&](const hir::UnpackedStructType& structure) {
            return std::ranges::any_of(
                structure.fields,
                [&](const hir::UnpackedAggregateField& member) {
                  return MemberTakesNewEvents(types, member);
                });
          },
          [](const hir::DynamicArrayType&) { return false; },
          [](const hir::QueueType&) { return false; },
          [](const hir::AssociativeArrayType&) { return false; },
          [](const hir::UnpackedUnionType&) { return false; },
          [](const hir::ScalarBitType&) { return false; },
          [](const hir::PackedArrayType&) { return false; },
          [](const hir::PackedStructType&) { return false; },
          [](const hir::PackedUnionType&) { return false; },
          [](const hir::EnumType&) { return false; },
          [](const hir::WildcardIndexType&) { return false; },
          [](const hir::StringType&) { return false; },
          [](const hir::DynamicType&) { return false; },
          [](const hir::RealType&) { return false; },
          [](const hir::ShortRealType&) { return false; },
          [](const hir::RealTimeType&) { return false; },
          [](const hir::ChandleType&) { return false; },
          [](const hir::ClassHandleType&) { return false; },
          [](const hir::ImportedClassHandleType&) { return false; },
          [](const hir::CovergroupHandleType&) { return false; },
          [](const hir::UnitObjectType&) { return false; },
          [](const hir::UnitObjectsType&) { return false; },
          [](const hir::VirtualInterfaceType&) { return false; },
          [](const hir::NullType&) { return false; },
          [](const hir::VoidType&) { return false; },
      });
}

namespace {

// The part a statement of the block it is handed writes, named in that block,
// since what names a part is a node of the block it stands in.
using PartIn = std::function<AccessPath(mir::Block&)>;

void GiveElementsNewEvents(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const PartIn& array,
    hir::TypeId element, mir::ExprId first, mir::ExprId count);

// Gives each event `type` holds at `part` a new object, in `frame`'s block: an
// element of an array at a time, by a loop over its positions. It is reached
// only for a type holding one, so the kinds of type that hold none have nothing
// to give.
void GiveNewEvents(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const PartIn& part,
    hir::TypeId type) {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const hir::TypePool& types = unit_lowerer.Hir().types;
  types.Get(type).Visit(
      Overloaded{
          [&](const hir::EventType&) {
            const mir::ExprId made = block.exprs.Add(
                mir::MakeCallExpr(
                    mir::Direct{.target = support::BuiltinFn::kEventNew}, {},
                    unit.builtins.event));
            block.AppendStmt(
                mir::ExprStmt{
                    .expr = block.exprs.Add(
                        BuildStoreExpr(unit, block, part(block), made))});
          },
          [&](const hir::UnpackedArrayType& array) {
            const std::uint64_t count =
                unit.types.Get(unit_lowerer.TranslateType(type))
                    .As<mir::UnpackedArrayType>()
                    ->Size();
            GiveElementsNewEvents(
                unit_lowerer, frame, part, array.element_type,
                mir::BuildIntLiteral(unit, block, 0),
                mir::BuildIntLiteral(
                    unit, block, static_cast<std::int64_t>(count)));
          },
          [&](const hir::UnpackedStructType& structure) {
            for (std::size_t i = 0; i < structure.fields.size(); ++i) {
              const hir::UnpackedAggregateField& member = structure.fields[i];
              if (!MemberTakesNewEvents(types, member)) {
                continue;
              }
              GiveNewEvents(
                  unit_lowerer, frame,
                  [&, i](mir::Block& at) {
                    return DescendInto(
                        part(at),
                        DescentStep{
                            .to =
                                StepToComponent{
                                    .position =
                                        base::ComponentIndex{
                                            static_cast<std::uint32_t>(i)}},
                            .part_type =
                                unit_lowerer.TranslateType(member.type)});
                  },
                  member.type);
            }
          },
          [](const hir::DynamicArrayType&) {},
          [](const hir::QueueType&) {},
          [](const hir::AssociativeArrayType&) {},
          [](const hir::UnpackedUnionType&) {},
          [](const hir::ScalarBitType&) {},
          [](const hir::PackedArrayType&) {},
          [](const hir::PackedStructType&) {},
          [](const hir::PackedUnionType&) {},
          [](const hir::EnumType&) {},
          [](const hir::WildcardIndexType&) {},
          [](const hir::StringType&) {},
          [](const hir::DynamicType&) {},
          [](const hir::RealType&) {},
          [](const hir::ShortRealType&) {},
          [](const hir::RealTimeType&) {},
          [](const hir::ChandleType&) {},
          [](const hir::ClassHandleType&) {},
          [](const hir::ImportedClassHandleType&) {},
          [](const hir::CovergroupHandleType&) {},
          [](const hir::UnitObjectType&) {},
          [](const hir::UnitObjectsType&) {},
          [](const hir::VirtualInterfaceType&) {},
          [](const hir::NullType&) {},
          [](const hir::VoidType&) {},
      });
}

// The same for `count` elements of the array at `array`, from the `int`
// position `first` on, an element at a time by a loop over their positions.
void GiveElementsNewEvents(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const PartIn& array,
    hir::TypeId element, mir::ExprId first, mir::ExprId count) {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId int_type = unit.builtins.int_type;
  const mir::LocalId from = DeclareLocal(frame, first);
  const mir::LocalId ordinal = frame.bindings->DeclareAnonymous(int_type);
  const mir::TypeId element_type = unit_lowerer.TranslateType(element);
  mir::Block body;
  GiveNewEvents(
      unit_lowerer, frame.WithBlock(&body),
      [&](mir::Block& at) {
        const mir::ExprId position = at.exprs.Add(
            mir::MakeBinary(
                unit, at, mir::BinaryOp::kAdd, at.ReadLocal(from, int_type),
                at.ReadLocal(ordinal, int_type), int_type));
        return DescendInto(
            array(at),
            DescentStep{
                .to =
                    StepToElement{
                        .position = BuildOrdinalPosition(unit, at, position)},
                .part_type = element_type});
      },
      element);
  block.AppendStmt(BuildCountingLoopStmt(
      unit, frame, block, count, ordinal,
      block.child_scopes.Add(std::move(body))));
}

}  // namespace

auto BuildDeclaredInitialValue(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, hir::TypeId type)
    -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId value_type = unit_lowerer.TranslateType(type);
  const mir::ExprId default_value =
      block.exprs.Add(BuildDefaultValueExpr(unit, block, value_type));
  if (!HoldsADeclaredEvent(unit_lowerer.Hir().types, type)) {
    return default_value;
  }
  const mir::LocalId held = DeclareLocal(frame, default_value);
  GiveNewEvents(
      unit_lowerer, frame,
      [&](mir::Block& at) {
        return AccessPath{
            .owner = at.LocalPlace(held, value_type), .descent = {}};
      },
      type);
  return block.ReadLocal(held, value_type);
}

void GiveElementsNewEvents(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::LocalId array,
    mir::TypeId array_type, hir::TypeId element, mir::ExprId first,
    mir::ExprId count) {
  GiveElementsNewEvents(
      unit_lowerer, frame,
      [&](mir::Block& at) {
        return AccessPath{
            .owner = at.LocalPlace(array, array_type), .descent = {}};
      },
      element, first, count);
}

}  // namespace lyra::lowering::hir_to_mir
