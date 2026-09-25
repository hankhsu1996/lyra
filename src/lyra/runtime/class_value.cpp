#include "lyra/runtime/class_value.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <ranges>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/support/member_layout.hpp"

namespace lyra::runtime {

static_assert(sizeof(MemberStorage) == support::kMemberSlotSize);
static_assert(
    offsetof(ObjectDefinition, first_member) == support::kFirstMemberAt);
static_assert(sizeof(ObjectDefinition::first_member) == sizeof(std::uint32_t));

auto ClassValue::MemberBytes(const ObjectDefinition* definition)
    -> std::size_t {
  return std::size_t{RequireDefinition(definition)->members.size} *
         sizeof(MemberStorage);
}

void ClassValue::operator delete(void* address) {
  ::operator delete(address);
}

ClassValue::ClassValue(
    const ObjectDefinition* definition, std::size_t holder_size)
    : members_(
          std::bit_cast<MemberStorage*>(
              std::bit_cast<std::uintptr_t>(this) + MembersAt(holder_size)),
          RequireDefinition(definition)->members.size) {
  AdoptClass(definition);
  // A value the runtime lays out is at the address of the base every entry
  // reaches it through, which is what lets one be recovered from that base
  // whether or not an allocation produced it -- an instance standing in the
  // design hierarchy is asked the same questions and no allocation made it.
  AdoptIdentity(this);
  for (auto [slot, descriptor] :
       std::views::zip(members_, definition->members.Descriptors())) {
    std::construct_at(&slot, descriptor);
    // Generated code reaches a member at its slot's own address, so the
    // storage has to begin where the slot does.
    if (slot.Address() != static_cast<void*>(&slot)) {
      throw InternalError(
          "ClassValue: a member's storage does not begin at its slot");
    }
  }
}

ClassValue::~ClassValue() {
  for (MemberStorage& slot : std::views::reverse(members_)) {
    std::destroy_at(&slot);
  }
}

auto ClassValue::Member(const ObjectDefinition* declared_by, std::uint32_t slot)
    -> void* {
  return members_[RequireDefinition(declared_by)->first_member + slot]
      .Address();
}

}  // namespace lyra::runtime
