#include "lyra/runtime/effect_storage.hpp"

#include <algorithm>
#include <cstddef>
#include <memory>
#include <new>
#include <span>

namespace lyra::runtime {

EffectStorage::~EffectStorage() {
  for (void* block : blocks_) {
    ::operator delete(block, std::align_val_t{kGranule});
  }
}

auto EffectStorage::ClassOf(std::size_t size, std::size_t alignment)
    -> std::size_t {
  if (size > kLargestClassBytes || alignment > kGranule) {
    return kNoClass;
  }
  return ((std::max(size, std::size_t{1}) + kGranule - 1) / kGranule) - 1;
}

auto EffectStorage::Allocate(std::size_t size, std::size_t alignment) -> void* {
  const std::size_t size_class = ClassOf(size, alignment);
  if (size_class == kNoClass) {
    return ::operator new(size, std::align_val_t{alignment});
  }
  if (free_.at(size_class) == nullptr) {
    Refill(size_class);
  }
  FreeNode* node = free_.at(size_class);
  free_.at(size_class) = node->next;
  return node;
}

void EffectStorage::Release(
    void* start, std::size_t size, std::size_t alignment) noexcept {
  const std::size_t size_class = ClassOf(size, alignment);
  if (size_class == kNoClass) {
    ::operator delete(start, std::align_val_t{alignment});
    return;
  }
  free_.at(size_class) = std::construct_at(
      static_cast<FreeNode*>(start), FreeNode{.next = free_.at(size_class)});
}

void EffectStorage::Refill(std::size_t size_class) {
  const std::size_t class_bytes = (size_class + 1) * kGranule;
  const std::size_t count = std::max(kBlockBytes / class_bytes, std::size_t{1});
  void* const block =
      ::operator new(count * class_bytes, std::align_val_t{kGranule});
  blocks_.push_back(block);
  const std::span<std::byte> bytes{
      static_cast<std::byte*>(block), count * class_bytes};
  for (std::size_t at = count; at > 0; --at) {
    void* const start = bytes.subspan((at - 1) * class_bytes).data();
    free_.at(size_class) = std::construct_at(
        static_cast<FreeNode*>(start), FreeNode{.next = free_.at(size_class)});
  }
}

}  // namespace lyra::runtime
