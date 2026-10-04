#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/published_method.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

struct ExternalClassId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ExternalClassId&) const
      -> std::strong_ordering = default;
};

// One property a class declares that another unit may name (LRM 8.5), as the
// unit declaring the class published it.
struct PublishedProperty {
  std::string name;
  TypeId type;

  auto operator==(const PublishedProperty&) const -> bool = default;
};

// A method of a class of another unit that overrides a virtual method one of
// its ancestors introduced (LRM 8.20) and gives it a body, and that method.
struct PromisedOverride {
  std::string method;
  ExternalDispatchSlot behavior;

  auto operator==(const PromisedOverride&) const -> bool = default;
};

using PublishedProperties = base::Arena<PublishedProperty, PublishedPropertyId>;

// Where the property named `name` sits among `properties`, or nothing where
// none carries that name.
[[nodiscard]] inline auto FindProperty(
    const PublishedProperties& properties, std::string_view name)
    -> std::optional<PublishedPropertyId> {
  for (const PublishedPropertyId id : properties.Ids()) {
    if (properties.Get(id).name == name) return id;
  }
  return std::nullopt;
}

// The method declared under `name` among `methods`, or nothing where the class
// declares none such. A class declares at most one method under a name.
[[nodiscard]] inline auto FindMethod(
    std::span<const PublishedMethod> methods, std::string_view name)
    -> const PublishedMethod* {
  for (const PublishedMethod& method : methods) {
    if (method.prototype.name == name) return &method;
  }
  return nullptr;
}

// Which of the virtual methods the class introduces the one named `name` is,
// counted over `methods` in order, or nothing where the class introduces none
// such -- which is the case for every class that overrides one instead.
[[nodiscard]] inline auto FindIntroducedVirtual(
    std::span<const PublishedMethod> methods, std::string_view name)
    -> std::optional<PublishedBehaviorId> {
  std::uint32_t ordinal = 0;
  for (const PublishedMethod& method : methods) {
    if (!std::holds_alternative<IntroducesVirtual>(method.dispatch)) {
      continue;
    }
    if (method.prototype.name == name) return PublishedBehaviorId{ordinal};
    ++ordinal;
  }
  return std::nullopt;
}

// A class of another unit this one reaches into, as that unit's signature
// published it, named by the unit that declares it and its canonical name. The
// lists below are ordered rather than sets, since a property's slot and a
// virtual method's ordinal are counted out of them. Every type here is this
// unit's own -- taken into its pool where the
// signature was consumed -- so nothing below this record reads a signature or a
// type it does not own.
//
// This unit compiles none of it; it holds what it compiled against. Every class
// this unit names has its signature read, because a value of one is laid out
// and converted to its views wherever it is held.
struct ExternalClass {
  std::string unit_name;
  std::string class_name;
  // The class it extends, as its own unit published, and absent where it
  // extends nothing. No property or method this class inherited is here;
  // reaching one is a walk along this chain, and each step is another unit's
  // signature consumed.
  std::optional<ExternalClassRef> base;
  bool is_interface_class = false;
  // The interface classes its declaration names (LRM 8.26.2), in the order
  // written. One it is by way of those, or of the class it extends, is read off
  // the signature of the class naming it.
  std::vector<ExternalClassRef> implements;
  // The properties another unit may name, in the order the class places them
  // at the start of its own storage.
  PublishedProperties properties;
  // The types of the `local` properties (LRM 8.18), placed after those. Nothing
  // outside the class names one; a class extending this one places its own
  // after them.
  std::vector<TypeId> local_property_types;
  // The properties of the class itself (LRM 8.9) another unit may name.
  std::vector<PublishedProperty> static_properties;
  // What a construction of the class is entered with (LRM 8.7); absent for an
  // interface class.
  std::optional<ExternalCalleeInterface> constructor;
  // Every method the class declares, in the order it declares them.
  std::vector<PublishedMethod> methods;
  // Each overriding method the class gives a body, with the virtual method it
  // overrides named by the class that introduced it. Resolved where the
  // signature is read, along the chain of signatures above it.
  std::vector<PromisedOverride> overrides;

  auto operator==(const ExternalClass&) const -> bool = default;
};

// The record kept of the class `class_name` of unit `unit_name`, or nothing
// where this unit holds no promise about it -- a class no signature the design
// compiles carries.
[[nodiscard]] inline auto FindExternalClass(
    std::span<const ExternalClass> records, std::string_view unit_name,
    std::string_view class_name) -> const ExternalClass* {
  for (const ExternalClass& record : records) {
    if (record.unit_name == unit_name && record.class_name == class_name) {
      return &record;
    }
  }
  return nullptr;
}

}  // namespace lyra::hir
