#include "lyra/lir/place_query.hpp"

#include <format>
#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lir {

namespace {

// The type the member `stated` names. The step says which declaration the
// member belongs to, and the front end settled that the value it runs on
// carries that declaration's members when it resolved the name (LRM 8.14), so
// the only thing left to disagree with itself here is the slot.
auto StatedMemberType(
    const CompilationUnit& unit, const StatedMemberRef& member) -> TypeId {
  const std::optional<MemberList> declared =
      DeclaredMembers(unit, member.declared_by);
  if (!declared) {
    throw InternalError(
        "lir: member projection names a declaration that declares no members");
  }
  if (member.slot.value >= declared->members.size()) {
    throw InternalError(
        std::format(
            "lir: member slot {} out of range on '{}'", member.slot.value,
            declared->owner));
  }
  return declared->members[member.slot.value].type;
}

}  // namespace

auto DeclaredMembers(const CompilationUnit& unit, TypeId type)
    -> std::optional<MemberList> {
  const std::optional<TypeDeclaration> declaration =
      unit.types.Get(type).Declaration();
  if (!declaration) {
    return std::nullopt;
  }
  return std::visit(
      Overloaded{
          [&](const ObjectType& object) -> std::optional<MemberList> {
            const Class& cls = unit.classes.Get(object.class_id);
            // A scope of the design hierarchy answers to no identifier, so what
            // a reader can be told about a bad step is the kind, as for a
            // closure.
            return MemberList{
                .members = cls.members,
                .owner = cls.name.has_value() ? std::string_view{*cls.name}
                                              : "a scope of the hierarchy"};
          },
          // What another unit published of its object is reached by performing
          // a behavior of the promise, so no step names a member of one and
          // this side holds no list to name one out of.
          [](const ExternalUnitObjectType&) -> std::optional<MemberList> {
            return std::nullopt;
          },
          [&](const CrossUnitClassType& cls) -> std::optional<MemberList> {
            const ExternalClass* record =
                FindExternalClass(unit, cls.unit_name, cls.class_name);
            if (record == nullptr) {
              return std::nullopt;
            }
            return MemberList{
                .members = record->members, .owner = record->class_name};
          },
          [&](const ClosureType& closure) -> std::optional<MemberList> {
            const Closure& decl = unit.closures.Get(closure.closure_id);
            // A closure carries no name of its own, so what a reader can be
            // told about a bad step is which kind of declaration it was on.
            return MemberList{.members = decl.captures, .owner = "a closure"};
          },
          [&](const StructType& record) -> std::optional<MemberList> {
            const Struct& decl = unit.structs.Get(record.struct_id);
            return MemberList{
                .members = decl.fields, .owner = "a gathered scope"};
          }},
      *declaration);
}

auto IsPlaceLocal(const Function& fn, const Operand& operand) -> bool {
  const auto* use = std::get_if<Use>(&operand);
  return use != nullptr && fn.values.Get(use->value).NamesStorage();
}

auto CallMakesValue(const CallTarget& target) -> bool {
  // Allocating a value cell answers the cell, which is storage the running
  // activation keeps, whatever type the cell is named by.
  if (const auto* cell = std::get_if<ValueCellTarget>(&target)) {
    return cell->op != ValueCellTarget::Op::kAllocate;
  }
  const auto* builtin = std::get_if<BuiltinTarget>(&target);
  if (builtin == nullptr) {
    return true;
  }
  switch (support::RuntimeEntryOf(builtin->fn).answer) {
    case support::EntryAnswer::kNewValue:
      return true;
    case support::EntryAnswer::kTheReceiver:
    case support::EntryAnswer::kPartOfTheReceiver:
      return false;
  }
  throw InternalError("lir: unknown entry answer");
}

auto MakesValue(const Function& fn, const InstrData& instr) -> bool {
  return std::visit(
      Overloaded{
          [](const CallInstr& call) { return CallMakesValue(call.target); },
          [&](const LoadInstr& load) {
            return !(
                IsPlaceLocal(fn, load.place.base) && load.place.chain.empty());
          },
          [](const CastInstr&) { return false; },
          [](const ProductInstr&) { return true; },
          [](const UnionInstr&) { return true; },
          [](const AggregateExtractInstr&) { return true; },
          [](const AggregateUpdateInstr&) { return true; },
          [](const BinaryInstr&) { return true; },
          [](const UnaryInstr&) { return true; },
          [](const ArrayInstr&) { return true; },
          [](const TagTestInstr&) { return true; },
          [](const AddrOfInstr&) { return true; },
          [](const StoreInstr&) { return false; },
          [](const ReceiveDepartureInstr&) { return true; }},
      instr);
}

auto PlaceType(
    const CompilationUnit& unit, const Function& fn, const Place& place)
    -> TypeId {
  const std::optional<TypeId> base = OperandType(fn, place.base);
  if (!base) {
    throw InternalError("lir: place base has no type");
  }
  const bool opens_with_deref =
      !place.chain.empty() &&
      std::holds_alternative<DerefProjection>(place.chain.front());
  if (!IsPlaceLocal(fn, place.base) && !opens_with_deref) {
    throw InternalError(
        "lir: a place over a value base must open with a dereference");
  }

  TypeId current = *base;
  for (const Projection& step : place.chain) {
    std::visit(
        Overloaded{
            [&](const DerefProjection&) {
              const std::optional<TypeId> target =
                  unit.types.Get(current).DerefTarget();
              if (!target) {
                throw InternalError(
                    "lir: dereference of a type that stands for no storage");
              }
              current = *target;
            },
            [&](const MemberProjection& projection) {
              current = StatedMemberType(unit, projection.member);
            }},
        step);
  }
  return current;
}

}  // namespace lyra::lir
