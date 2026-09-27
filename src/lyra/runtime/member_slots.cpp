#include "lyra/runtime/member_slots.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <ranges>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/support/member_layout.hpp"

namespace lyra::runtime {

static_assert(sizeof(MemberStorage) == support::kMemberSlotSize);

auto MemberSlots::Allocate(std::size_t holder_size, MemberStorageSchema schema)
    -> void* {
  return ::operator new(
      At(holder_size) + (std::size_t{schema.size} * sizeof(MemberStorage)));
}

void MemberSlots::Release(void* address) {
  ::operator delete(address);
}

MemberSlots::MemberSlots(
    const void* holder, std::size_t holder_size, MemberStorageSchema schema)
    : slots_(
          std::bit_cast<MemberStorage*>(
              std::bit_cast<std::uintptr_t>(holder) + At(holder_size)),
          schema.size) {
  for (auto [slot, descriptor] :
       std::views::zip(slots_, schema.Descriptors())) {
    std::construct_at(&slot, descriptor);
    // Generated code reaches a member at its slot's own address, so the
    // storage has to begin where the slot does.
    if (slot.Address() != static_cast<void*>(&slot)) {
      throw InternalError(
          "MemberSlots: a member's storage does not begin at its slot");
    }
  }
}

MemberSlots::~MemberSlots() {
  for (MemberStorage& slot : std::views::reverse(slots_)) {
    std::destroy_at(&slot);
  }
}

}  // namespace lyra::runtime
