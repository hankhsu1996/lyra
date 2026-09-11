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

namespace lyra::lir {

namespace {

// The class the declaration `type` names extends, or nothing where the lineage
// ends. One walk covers both sides of the unit boundary: a class this unit
// compiles states its base outright, and one another unit declares states it on
// the promise this unit read.
auto ExtendedBy(const CompilationUnit& unit, TypeId type)
    -> std::optional<TypeId> {
  return unit.types.Get(type).Visit(
      Overloaded{
          [&](const ObjectType& object) -> std::optional<TypeId> {
            const std::optional<Base>& base =
                unit.classes.Get(object.class_id).base;
            return base.has_value() ? BaseType(unit, *base) : std::nullopt;
          },
          [&](const CrossUnitClassType& cls) -> std::optional<TypeId> {
            const ExternalClass* record =
                FindExternalClass(unit, cls.unit_name, cls.class_name);
            if (record == nullptr || !record->base.has_value()) {
              return std::nullopt;
            }
            return BaseType(unit, Base{*record->base});
          },
          [](const auto&) -> std::optional<TypeId> { return std::nullopt; }});
}

// The type the member `stated` names, reached on a place that has arrived at
// `carrier`. Every failure here is this artifact disagreeing with itself: the
// step names a declaration, so a step naming one the place has not reached, or
// a slot that declaration does not have, could only have been built wrongly.
auto StatedMemberType(
    const CompilationUnit& unit, TypeId carrier, const StatedMemberRef& member)
    -> TypeId {
  const std::optional<MemberList> declared =
      DeclaredMembers(unit, member.declared_by);
  if (!declared) {
    throw InternalError(
        "lir: member projection names a declaration that declares no members");
  }
  if (!CarriesMembersOf(unit, carrier, member.declared_by)) {
    throw InternalError(
        std::format(
            "lir: member step names '{}', which the place has not reached a "
            "carrier of",
            declared->owner));
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

auto CarriesMembersOf(
    const CompilationUnit& unit, TypeId type, TypeId declaration) -> bool {
  for (std::optional<TypeId> at = type; at.has_value();
       at = ExtendedBy(unit, *at)) {
    if (*at == declaration) {
      return true;
    }
  }
  return false;
}

auto DeclaredMembers(const CompilationUnit& unit, TypeId type)
    -> std::optional<MemberList> {
  return unit.types.Get(type).Visit(
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
          [&](const ExternalUnitObjectType& external)
              -> std::optional<MemberList> {
            const ExternalUnitObject& object =
                unit.external_unit_objects.Get(external.object);
            return MemberList{
                .members = object.members, .owner = object.class_name};
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
          },
          [](const auto&) -> std::optional<MemberList> {
            return std::nullopt;
          }});
}

auto IsPlaceLocal(const Function& fn, const Operand& operand) -> bool {
  const auto* use = std::get_if<Use>(&operand);
  return use != nullptr && fn.values.Get(use->value).NamesStorage();
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
              current = std::visit(
                  Overloaded{
                      [&](const StatedMemberRef& member) -> TypeId {
                        return StatedMemberType(unit, current, member);
                      },
                      // The declaration is one nothing here names, so there is
                      // no member list to count against and the step states
                      // what it reaches.
                      [](const SuppliedMemberRef& member) -> TypeId {
                        return member.reached;
                      }},
                  projection.member);
            }},
        step);
  }
  return current;
}

}  // namespace lyra::lir
