#include "lyra/lowering/hir_to_mir/equivalence_identity.hpp"

#include <format>
#include <optional>
#include <string>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/imported_runtime_class.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto DeclarationText(std::string_view unit_name, const support::DefPath& path)
    -> std::string {
  return std::format("{}::{}", unit_name, support::DisplayOf(path));
}

}  // namespace

auto EquivalenceIdentityOf(const UnitLowerer& unit_lowerer, hir::TypeId type)
    -> std::string {
  const hir::CompilationUnit& hir_unit = unit_lowerer.Hir();
  // An integral type that is no enumeration is equivalent to every other of its
  // width, its states and its signedness, however its range or its members are
  // written (LRM 6.22.2 c), which is what the type this unit translates it to
  // states and nothing more.
  const auto integral = [&] {
    const auto& translated = unit_lowerer.Unit()
                                 .types.Get(unit_lowerer.TranslateType(type))
                                 .Get<mir::IntegralType>();
    return std::format(
        "{}{} [{}:0]",
        translated.state_kind == mir::IntegralStateKind::kFourState ? "logic"
                                                                    : "bit",
        translated.signedness == mir::Signedness::kSigned ? " signed" : "",
        translated.bit_width - 1);
  };
  const auto of_element = [&](hir::TypeId element, std::string_view dimension) {
    return std::format(
        "{} {}", EquivalenceIdentityOf(unit_lowerer, element), dimension);
  };
  const auto no_value = [](std::string_view what) -> std::string {
    throw InternalError(
        std::format(
            "EquivalenceIdentityOf: {} is no type a value of any type is made "
            "of -- please report this as a bug",
            what));
  };
  return hir_unit.types.Get(type).Visit(
      Overloaded{
          [&](const hir::ScalarBitType&) { return integral(); },
          [&](const hir::PackedArrayType&) { return integral(); },
          [&](const hir::PackedStructType&) { return integral(); },
          [&](const hir::PackedUnionType&) { return integral(); },
          // An enumeration, a structure and a union are each equivalent only
          // to themselves (LRM 6.22.1 d, 6.22.2 b).
          [&](const hir::EnumType& t) {
            return "enum " +
                   DeclarationText(t.declaration.unit_name, t.declaration.path);
          },
          [&](const hir::UnpackedStructType& t) {
            return "struct " +
                   DeclarationText(t.declaration.unit_name, t.declaration.path);
          },
          [&](const hir::UnpackedUnionType& t) {
            return "union " +
                   DeclarationText(t.declaration.unit_name, t.declaration.path);
          },
          // A fixed-size array is its element and its count, whatever range
          // gives the count (LRM 6.22.2 d); every other array its kind, its
          // element and, for an associative one, its index (LRM 6.22.2 e), so
          // a queue is one whatever bound it was declared with.
          [&](const hir::UnpackedArrayType& t) {
            return of_element(
                t.element_type, std::format("[{}]", t.dim.ElementCount()));
          },
          [&](const hir::DynamicArrayType& t) {
            return of_element(t.element_type, "[]");
          },
          [&](const hir::QueueType& t) {
            return of_element(t.element_type, "[$]");
          },
          [&](const hir::AssociativeArrayType& t) {
            return of_element(
                t.element_type,
                std::format(
                    "[{}]", EquivalenceIdentityOf(unit_lowerer, t.key_type)));
          },
          [](const hir::WildcardIndexType&) -> std::string { return "*"; },
          [](const hir::StringType&) -> std::string { return "string"; },
          [](const hir::EventType&) -> std::string { return "event"; },
          // `realtime` is `real` (LRM 6.12).
          [](const hir::RealType&) -> std::string { return "real"; },
          [](const hir::RealTimeType&) -> std::string { return "real"; },
          [](const hir::ShortRealType&) -> std::string { return "shortreal"; },
          [](const hir::ChandleType&) -> std::string { return "chandle"; },
          [](const hir::NullType&) -> std::string { return "null"; },
          // A class handle is equivalent only to a handle of its own class
          // (LRM 6.22.1 d).
          [&](const hir::ClassHandleType& t) {
            const hir::ClassKey& key = hir_unit.classes.KeyOf(t.class_id);
            return "class " + DeclarationText(key.unit_name, key.class_path);
          },
          [](const hir::ImportedClassHandleType& t) {
            return std::format(
                "class std::{}", support::ImportedRuntimeClassName(t.klass));
          },
          [&](const hir::WeakHandleType&) {
            return no_value("a weakly held handle");
          },
          [&](const hir::CovergroupHandleType& t) {
            const hir::CovergroupKey& key =
                hir_unit.covergroups.KeyOf(t.covergroup);
            return "covergroup " + DeclarationText(key.unit_name, key.path);
          },
          [](const hir::VirtualInterfaceType& t) {
            return "virtual " + t.unit_name;
          },
          [&](const hir::UnitObjectType&) { return no_value("an instance"); },
          [&](const hir::UnitObjectsType&) {
            return no_value("a set of instances");
          },
          [&](const hir::DynamicType&) {
            return no_value("a value of any type");
          },
          [&](const hir::VoidType&) { return no_value("'void'"); }});
}

auto WithoutQueueBounds(const mir::TypePool& types, mir::TypeId type)
    -> mir::TypeId {
  const auto element = [&](mir::TypeId of) {
    return WithoutQueueBounds(types, of);
  };
  const mir::Type& held = types.Get(type);
  if (const auto* array = held.As<mir::UnpackedArrayType>()) {
    return types.Intern(
        mir::Type{mir::UnpackedArrayType{
            .element_type = element(array->element_type),
            .count = array->count}});
  }
  if (const auto* array = held.As<mir::DynamicArrayType>()) {
    return types.Intern(
        mir::Type{mir::DynamicArrayType{
            .element_type = element(array->element_type)}});
  }
  if (const auto* queue = held.As<mir::QueueType>()) {
    return types.Intern(
        mir::Type{mir::QueueType{
            .element_type = element(queue->element_type),
            .max_bound = std::nullopt}});
  }
  if (const auto* array = held.As<mir::AssociativeArrayType>()) {
    return types.Intern(
        mir::Type{mir::AssociativeArrayType{
            .element_type = element(array->element_type),
            .key_type = element(array->key_type)}});
  }
  return type;
}

auto HoldsBoundedQueueBelow(const mir::TypePool& types, mir::TypeId type)
    -> bool {
  const std::optional<mir::TypeId> element =
      types.Get(type).ContainerElementType();
  return element.has_value() && *element != WithoutQueueBounds(types, *element);
}

}  // namespace lyra::lowering::hir_to_mir
