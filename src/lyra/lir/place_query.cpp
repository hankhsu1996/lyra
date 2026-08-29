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

auto DeclaredMembers(const CompilationUnit& unit, TypeId type)
    -> std::optional<MemberList> {
  return std::visit(
      Overloaded{
          [&](const ObjectType& object) -> std::optional<MemberList> {
            const Class& cls = unit.classes.Get(object.class_id);
            return MemberList{.members = cls.members, .owner = cls.name};
          },
          [&](const ExternalUnitObjectType& external)
              -> std::optional<MemberList> {
            const ExternalUnitObject& object =
                unit.external_unit_objects.Get(external.object);
            return MemberList{
                .members = object.members, .owner = object.class_name};
          },
          [&](const ClosureType& closure) -> std::optional<MemberList> {
            const Closure& decl = unit.closures.Get(closure.closure_id);
            return MemberList{.members = decl.captures, .owner = decl.name};
          },
          [](const auto&) -> std::optional<MemberList> {
            return std::nullopt;
          }},
      unit.types.Get(type).data);
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
                  DerefTarget(unit.types, current);
              if (!target) {
                throw InternalError(
                    "lir: dereference of a type that stands for no storage");
              }
              current = *target;
            },
            [&](const MemberProjection& m) {
              const std::optional<MemberList> declared =
                  DeclaredMembers(unit, current);
              if (!declared) {
                throw InternalError(
                    "lir: member projection on a base that declares no "
                    "members");
              }
              if (m.member.value >= declared->members.size()) {
                throw InternalError(
                    std::format(
                        "lir: member index {} out of range on '{}'",
                        m.member.value, declared->owner));
              }
              current = declared->members[m.member.value].type;
            }},
        step);
  }
  return current;
}

}  // namespace lyra::lir
