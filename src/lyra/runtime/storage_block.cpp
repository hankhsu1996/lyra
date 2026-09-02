#include "lyra/runtime/storage_block.hpp"

#include <cstdint>
#include <memory>
#include <span>

#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/scope_program.hpp"

namespace lyra::runtime {

StorageBlock::StorageBlock(MemberStorageSchema schema) {
  const std::span<const MemberStorageDescriptor> descriptors =
      schema.Descriptors();
  slots_.reserve(descriptors.size());
  for (const MemberStorageDescriptor& descriptor : descriptors) {
    slots_.push_back(std::make_unique<MemberStorage>(descriptor));
  }
}

auto StorageBlock::Address(std::uint32_t index) -> void* {
  return slots_.at(index)->Address();
}

auto StorageBlock::Held(std::uint32_t index) -> void* {
  return slots_.at(index)->HeldValue();
}

void StorageBlock::Adopt(std::uint32_t index, void* handle) {
  slots_.at(index)->AdoptFrom(handle);
}

auto StorageBlock::Size() const -> std::uint32_t {
  return static_cast<std::uint32_t>(slots_.size());
}

}  // namespace lyra::runtime
