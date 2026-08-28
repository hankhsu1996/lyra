#include "lyra/lir/place_query.hpp"

#include <format>
#include <optional>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lir/type_query.hpp"

namespace lyra::lir {

namespace {

// The members of whatever object the chain has reached, and what to call it in
// a message. A member step names a position the same way whether the object is
// a class this unit compiles or one another unit published.
struct ObjectMembers {
  std::string_view name;
  std::span<const Member> members;
};

auto MembersOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<ObjectMembers> {
  return std::visit(
      Overloaded{
          [&](const ObjectType& object) -> std::optional<ObjectMembers> {
            const Class& cls = unit.classes.Get(object.class_id);
            return ObjectMembers{.name = cls.name, .members = cls.members};
          },
          [&](const ExternalUnitObjectType& external)
              -> std::optional<ObjectMembers> {
            const ExternalUnitObject& object =
                unit.external_unit_objects.Get(external.object);
            return ObjectMembers{
                .name = object.class_name, .members = object.members};
          },
          [](const auto&) -> std::optional<ObjectMembers> {
            return std::nullopt;
          }},
      unit.types.Get(type).data);
}

}  // namespace

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
                  DerefTarget(unit.types, current);
              if (!target) {
                throw InternalError(
                    "lir: dereference of a type that stands for no storage");
              }
              current = *target;
            },
            [&](const MemberProjection& m) {
              const std::optional<ObjectMembers> object =
                  MembersOf(unit, current);
              if (!object) {
                throw InternalError(
                    "lir: member projection on a non-object base");
              }
              if (m.member.value >= object->members.size()) {
                throw InternalError(
                    std::format(
                        "lir: member index {} out of range on class '{}'",
                        m.member.value, object->name));
              }
              current = object->members[m.member.value].type;
            }},
        step);
  }
  return current;
}

}  // namespace lyra::lir
