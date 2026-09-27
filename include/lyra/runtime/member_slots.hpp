#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/scope_program.hpp"

namespace lyra::runtime {

// The member slots a value holds right after itself, in the allocation it was
// made in: one per descriptor of its schema, in schema order and all one size,
// so generated code reaches a slot at a fixed distance from the value's address
// without asking. The value owns them and ends them with itself.
class MemberSlots {
 public:
  // Where the slots of a value of a kind `holder_size` bytes long begin: at the
  // first slot boundary after it.
  static constexpr auto At(std::size_t holder_size) -> std::size_t {
    constexpr std::size_t kAlign = alignof(MemberStorage);
    return (holder_size + kAlign - 1) / kAlign * kAlign;
  }

  // An allocation with room for a value of `holder_size` bytes and the slots
  // `schema` describes after it, which the value is built into.
  [[nodiscard]] static auto Allocate(
      std::size_t holder_size, MemberStorageSchema schema) -> void*;
  static void Release(void* address);

  // Builds the slots `schema` describes after the value at `holder`, which is
  // `holder_size` bytes long and was made by the allocation above.
  MemberSlots(
      const void* holder, std::size_t holder_size, MemberStorageSchema schema);
  MemberSlots(const MemberSlots&) = delete;
  auto operator=(const MemberSlots&) -> MemberSlots& = delete;
  MemberSlots(MemberSlots&&) = delete;
  auto operator=(MemberSlots&&) -> MemberSlots& = delete;
  ~MemberSlots();

  [[nodiscard]] auto operator[](std::uint32_t index) -> MemberStorage& {
    return slots_[index];
  }

  [[nodiscard]] auto Size() const -> std::uint32_t {
    return static_cast<std::uint32_t>(slots_.size());
  }

 private:
  std::span<MemberStorage> slots_;
};

}  // namespace lyra::runtime
