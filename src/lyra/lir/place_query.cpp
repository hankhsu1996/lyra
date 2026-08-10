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
#include "lyra/lir/type_query.hpp"

namespace lyra::lir {

namespace {

auto AsClassId(const CompilationUnit& unit, TypeId type)
    -> std::optional<ClassId> {
  const auto* object = std::get_if<ObjectType>(&unit.types.Get(type).data);
  return object != nullptr ? std::optional{object->class_id} : std::nullopt;
}

auto IsPlaceLocal(const Function& fn, const Operand& op) -> bool {
  const auto* use = std::get_if<Use>(&op);
  return use != nullptr && fn.values.Get(use->value).kind == LocalKind::kPlace;
}

}  // namespace

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
              const std::optional<ClassId> class_id = AsClassId(unit, current);
              if (!class_id) {
                throw InternalError(
                    "lir: member projection on a non-object base");
              }
              const Class& cls = unit.classes.Get(*class_id);
              if (m.member.value >= cls.members.size()) {
                throw InternalError(
                    std::format(
                        "lir: member index {} out of range on class '{}'",
                        m.member.value, cls.name));
              }
              current = cls.members[m.member.value].type;
            }},
        step);
  }
  return current;
}

}  // namespace lyra::lir
