#include "lyra/runtime/frame_allocator.hpp"

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <new>
#include <span>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

namespace {

auto IsPowerOfTwo(uint64_t v) -> bool {
  return v != 0 && (v & (v - 1)) == 0;
}

auto CheckedAlignUp(uint64_t value, uint64_t align) -> uint64_t {
  uint64_t mask = align - 1;
  if (value > UINT64_MAX - mask) {
    throw common::InternalError(
        "AllocateProcessFrames", "overflow in alignment computation");
  }
  return (value + mask) & ~mask;
}

auto CheckedAdd(uint64_t a, uint64_t b) -> uint64_t {
  if (a > UINT64_MAX - b) {
    throw common::InternalError(
        "AllocateProcessFrames", "packed size overflow during accumulation");
  }
  return a + b;
}

constexpr uint64_t kMinBaseAlignment = alignof(std::max_align_t);

// Sole allocation primitive for packed process state buffers.
//
// Memory layout:
//   [header: alloc_align bytes] [data: data_size bytes]
//
// The last sizeof(uint64_t) bytes of the header store alloc_align,
// so LyraDestroyProcessStates can recover the alignment from only
// the data pointer.
auto AllocatePackedBuffer(uint64_t alloc_align, uint64_t data_size) -> void* {
  static constexpr size_t kTagSize = sizeof(uint64_t);
  uint64_t total = CheckedAdd(alloc_align, data_size);
  auto align_val = std::align_val_t{alloc_align};
  auto* raw =
      static_cast<char*>(::operator new(total, align_val, std::nothrow));
  if (raw == nullptr) {
    throw common::InternalError("AllocateProcessFrames", "allocation failed");
  }
  auto buffer = std::span(raw, total);
  auto* data = buffer.subspan(alloc_align).data();
  auto tag = buffer.subspan(alloc_align - kTagSize, kTagSize);
  std::memcpy(tag.data(), &alloc_align, kTagSize);
  std::memset(data, 0, data_size);
  return data;
}

}  // namespace

auto AllocateProcessFrames(
    std::span<void*> states_out, std::span<const uint32_t> schema_indices,
    std::span<const ProcessStateSchema> schemas) -> void* {
  if (states_out.size() != schema_indices.size()) {
    throw common::InternalError(
        "AllocateProcessFrames", "states_out / schema_indices size mismatch");
  }

  uint32_t num = static_cast<uint32_t>(schema_indices.size());
  if (num == 0) {
    return nullptr;
  }

  // Pass 1: validate and compute total packed buffer size.
  uint64_t total_size = 0;
  uint64_t max_align = 1;

  for (uint32_t idx : schema_indices) {
    if (idx >= schemas.size()) {
      throw common::InternalError(
          "AllocateProcessFrames", "schema_index out of range");
    }
    const auto& schema = schemas[idx];
    if (!IsPowerOfTwo(schema.state_align)) {
      throw common::InternalError(
          "AllocateProcessFrames", "invalid state_align");
    }
    max_align = std::max(max_align, schema.state_align);
    total_size = CheckedAlignUp(total_size, schema.state_align);
    total_size = CheckedAdd(total_size, schema.state_size);
  }

  uint64_t alloc_align = std::max(max_align, kMinBaseAlignment);
  uint64_t alloc_size =
      CheckedAlignUp(std::max(total_size, uint64_t{1}), alloc_align);

  // Pass 2: allocate and zero.
  auto* data =
      static_cast<char*>(AllocatePackedBuffer(alloc_align, alloc_size));

  // Pass 3: distribute state pointers and call frame_init.
  uint64_t offset = 0;
  auto buffer = std::span(data, alloc_size);
  for (uint32_t i = 0; i < num; ++i) {
    const auto& schema = schemas[schema_indices[i]];
    offset = CheckedAlignUp(offset, schema.state_align);
    states_out[i] = buffer.subspan(offset).data();
    if (schema.frame_init != nullptr) {
      schema.frame_init(states_out[i]);
    }
    offset += schema.state_size;
  }

  return data;
}

void FreePackedBuffer(void* packed_buffer) {
  if (packed_buffer == nullptr) return;
  // Recover the original allocation base from the alignment tag stored
  // just before the data pointer. This is the deallocation counterpart
  // to AllocatePackedBuffer's header layout:
  //   [header: alignment bytes] [data bytes]
  //   tag at header[alignment - 8]
  // Backward pointer navigation from the data pointer to the tag and
  // base is inherent to this allocator format.
  static constexpr size_t kTagSize = sizeof(uint64_t);
  auto* data = static_cast<char*>(packed_buffer);
  uint64_t alignment = 0;
  std::memcpy(&alignment, data - kTagSize, kTagSize);
  ::operator delete(data - alignment, std::align_val_t{alignment});
}

}  // namespace lyra::runtime
