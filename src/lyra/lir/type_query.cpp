#include "lyra/lir/type_query.hpp"

#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

auto Pointee(const TypeArena& types, TypeId type) -> std::optional<TypeId> {
  return std::visit(
      Overloaded{
          [](const PointerType& p) -> std::optional<TypeId> {
            return p.pointee;
          },
          [](const RefType& r) -> std::optional<TypeId> { return r.pointee; },
          [](const ManagedRefType& m) -> std::optional<TypeId> {
            return m.pointee;
          },
          [](const auto&) -> std::optional<TypeId> { return std::nullopt; }},
      types.Get(type).data);
}

auto IsAddressOnly(const TypeArena& types, TypeId type) -> bool {
  return std::visit(
      Overloaded{
          [](const ObservableType&) { return true; },
          [](const ResolvedType&) { return true; },
          [](const DriverType&) { return true; },
          [](const ObjectType&) { return true; },
          [](const ExternalUnitObjectType&) { return true; },
          [](const ExternalClassType&) { return true; },
          [](const auto&) { return false; }},
      types.Get(type).data);
}

auto PackedShape(const TypeArena& types, TypeId type)
    -> const PackedArrayType& {
  const TypeData& data = types.Get(type).data;
  if (const auto* packed = std::get_if<PackedArrayType>(&data)) {
    return *packed;
  }
  if (const auto* enumeration = std::get_if<EnumType>(&data)) {
    return enumeration->base;
  }
  throw InternalError("lir: type has no packed shape; it is not integral");
}

auto IsCoroutine(const TypeArena& types, TypeId type) -> bool {
  return std::holds_alternative<CoroutineType>(types.Get(type).data);
}

}  // namespace lyra::lir
