#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>

#include "lyra/base/arena.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/published_behavior.hpp"
#include "lyra/hir/published_member.hpp"

namespace lyra::hir {

struct ExternalClassId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ExternalClassId&) const
      -> std::strong_ordering = default;
};

// A class of another unit this one reaches into, as that unit's signature
// promised it, named by the unit that declares it and its canonical name. The
// two lists below are ordered rather than sets: a property's slot and a
// behavior's ordinal are counted out of them. Every type here is this unit's
// own -- taken into its pool where the signature was consumed -- so nothing
// below this record reads a signature or a type it does not own.
//
// This unit compiles none of it; it holds what it compiled against. Naming such
// a class and holding what it published are two different acts: a handle typed
// by one names it and needs none of this, while reaching a property or a
// behavior on one is what consumes the promise.
struct ExternalClass {
  std::string unit_name;
  std::string class_name;
  // The class it extends, as its own unit promised, and absent where it extends
  // nothing. What this class inherited is nowhere here: reaching an inherited
  // property or behavior is a walk along this chain, and each step is another
  // unit's promise consumed.
  std::optional<ExternalClassRef> base;
  bool is_interface_class = false;
  base::Arena<PublishedMember, PublishedMemberId> members;
  base::Arena<PublishedBehavior, PublishedBehaviorId> behaviors;

  // The property published under `name`, or nothing when the class published no
  // such name. A name with no answer here is one the class never promised, so a
  // reference to it has nothing to compile against.
  [[nodiscard]] auto FindMember(std::string_view name) const
      -> std::optional<PublishedMemberId> {
    for (const PublishedMemberId id : members.Ids()) {
      if (members.Get(id).name == name) return id;
    }
    return std::nullopt;
  }

  // Which of this class's introductions the behavior named `name` is, or
  // nothing where this class introduces none such -- which is the case for
  // every class that answers a behavior an ancestor introduced.
  [[nodiscard]] auto FindBehavior(std::string_view name) const
      -> std::optional<PublishedBehaviorId> {
    for (const PublishedBehaviorId id : behaviors.Ids()) {
      if (behaviors.Get(id).name == name) return id;
    }
    return std::nullopt;
  }
};

// The record kept of the class `class_name` of unit `unit_name`, or nothing
// where this unit consumed no promise about it -- which is the state of every
// class it merely names.
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
