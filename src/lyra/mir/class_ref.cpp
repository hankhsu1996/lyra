#include "lyra/mir/class_ref.hpp"

#include <format>
#include <optional>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

auto IntroducesSlot(const std::optional<VirtualDispatchRole>& role) -> bool {
  if (!role.has_value()) {
    return false;
  }
  return std::visit(
      Overloaded{
          [](const IntroducesVirtualSlot&) { return true; },
          [](const OverridesIntraUnitSlot&) { return false; },
          [](const OverridesExternalSlot&) { return false; },
          [](const OverridesLibraryVirtual&) { return false; }},
      *role);
}

auto ClassOfObject(const TypePool& types, TypeId object) -> DeclaredClassRef {
  const auto refers_to_no_class =
      [](std::string_view what) -> DeclaredClassRef {
    throw InternalError(
        std::format(
            "mir: an object built of {} names no class -- please report this "
            "as a bug",
            what));
  };
  const auto not_an_object = [&]() -> DeclaredClassRef {
    return refers_to_no_class("something that is not an object");
  };
  return types.Get(object).Visit(
      Overloaded{
          [](const ObjectType& o) -> DeclaredClassRef {
            return IntraUnitClassRef{.class_id = o.class_id};
          },
          [](const CrossUnitClassType& c) -> DeclaredClassRef {
            return CrossUnitClassRef{
                .unit_name = c.unit_name, .class_name = c.class_name};
          },
          // An object this unit carries no class identity for: one reached
          // past another unit's signature, one the runtime library defines,
          // and one another unit's design element declares.
          [&](const OpaqueObjectType&) {
            return refers_to_no_class("an object with no class to name");
          },
          [&](const RuntimeClassType&) {
            return refers_to_no_class("an object of a runtime class");
          },
          [&](const ExternalUnitObjectType&) {
            return refers_to_no_class("another unit's object");
          },
          [&](const PackedArrayType&) { return not_an_object(); },
          [&](const EnumType&) { return not_an_object(); },
          [&](const UnpackedArrayType&) { return not_an_object(); },
          [&](const DynamicArrayType&) { return not_an_object(); },
          [&](const QueueType&) { return not_an_object(); },
          [&](const AssociativeArrayType&) { return not_an_object(); },
          [&](const WildcardIndexType&) { return not_an_object(); },
          [&](const StringType&) { return not_an_object(); },
          [&](const MachineCStringType&) { return not_an_object(); },
          [&](const MachineBoolType&) { return not_an_object(); },
          [&](const MachineIntType&) { return not_an_object(); },
          [&](const MachineFloatType&) { return not_an_object(); },
          [&](const MachineArrayType&) { return not_an_object(); },
          [&](const MachineFunctionType&) { return not_an_object(); },
          [&](const EventType&) { return not_an_object(); },
          [&](const RealType&) { return not_an_object(); },
          [&](const ShortRealType&) { return not_an_object(); },
          [&](const ChandleType&) { return not_an_object(); },
          [&](const VoidType&) { return not_an_object(); },
          [&](const EmptyType&) { return not_an_object(); },
          [&](const RuntimeEffectsType&) { return not_an_object(); },
          [&](const FilesType&) { return not_an_object(); },
          [&](const DiagnosticType&) { return not_an_object(); },
          [&](const RuntimeLibraryType&) { return not_an_object(); },
          [&](const CoroutineType&) { return not_an_object(); },
          [&](const RefType&) { return not_an_object(); },
          [&](const PointerType&) { return not_an_object(); },
          [&](const ManagedRefType&) { return not_an_object(); },
          [&](const VectorType&) { return not_an_object(); },
          [&](const TupleType&) { return not_an_object(); },
          [&](const UnionType&) { return not_an_object(); },
          [&](const TaggedUnionType&) { return not_an_object(); },
          [&](const ObservableType&) { return not_an_object(); },
          [&](const ResolvedType&) { return not_an_object(); },
          [&](const DriverType&) { return not_an_object(); },
          [&](const OpenWriteType&) { return not_an_object(); },
          [&](const DesignationType&) { return not_an_object(); },
          [&](const SampledHistoryType&) { return not_an_object(); },
          [&](const EvaluationAttemptsType&) { return not_an_object(); },
          [&](const StructType&) { return not_an_object(); },
          [&](const ClosureType&) { return not_an_object(); }});
}

}  // namespace lyra::mir
