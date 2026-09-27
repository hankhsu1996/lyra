#include "lyra/runtime/class_value.hpp"

#include <cstddef>
#include <cstdint>

#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/member_slots.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/support/member_layout.hpp"

namespace lyra::runtime {

static_assert(
    offsetof(ObjectDefinition, first_member) == support::kFirstMemberAt);
static_assert(sizeof(ObjectDefinition::first_member) == sizeof(std::uint32_t));

auto ClassValue::SchemaOf(const ObjectDefinition* definition)
    -> MemberStorageSchema {
  return RequireDefinition(definition)->members;
}

void ClassValue::operator delete(void* address) {
  MemberSlots::Release(address);
}

ClassValue::ClassValue(
    const ObjectDefinition* definition, std::size_t holder_size)
    : members_(this, holder_size, SchemaOf(definition)) {
  AdoptClass(definition);
  // A value the runtime lays out is at the address of the base every entry
  // reaches it through, which is what lets one be recovered from that base
  // whether or not an allocation produced it -- an instance standing in the
  // design hierarchy is asked the same questions and no allocation made it.
  AdoptIdentity(this);
}

ClassValue::~ClassValue() = default;

auto ClassValue::Member(const ObjectDefinition* declared_by, std::uint32_t slot)
    -> void* {
  return members_[RequireDefinition(declared_by)->first_member + slot]
      .Address();
}

}  // namespace lyra::runtime
