#include "lyra/runtime/process_state_constructor.hpp"

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <memory>
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
        "LyraConstructProcessStates", "overflow in alignment computation");
  }
  return (value + mask) & ~mask;
}

auto CheckedAdd(uint64_t a, uint64_t b) -> uint64_t {
  if (a > UINT64_MAX - b) {
    throw common::InternalError(
        "LyraConstructProcessStates",
        "packed size overflow during accumulation");
  }
  return a + b;
}

// Floor for the allocation base alignment.
// operator new with std::align_val_t requires a valid alignment value.
// We use alignof(std::max_align_t) as the floor to ensure the base
// pointer satisfies all fundamental type alignment requirements.
// This floor applies only to the allocation base, not to packing layout.
// Schema-declared alignments below this floor are still honored for
// offset computation within the packed buffer.
constexpr uint64_t kMinBaseAlignment = alignof(std::max_align_t);

// Sole allocation primitive for packed process state buffers.
//
// Memory layout:
//   [header: alloc_align bytes] [data: data_size bytes]
//   ^                           ^
//   base (from operator new)    data pointer (returned to caller)
//
// The last sizeof(uint64_t) bytes of the header store alloc_align,
// so Free() can recover the alignment from only the data pointer.
// alloc_align >= kMinBaseAlignment >= sizeof(uint64_t), so the header
// always has room.
//
// Ownership contract:
//   - Constructor allocates via operator new with std::align_val_t.
//   - Release() transfers ownership to the caller. The caller must
//     eventually pass the data pointer to Free() (via
//     LyraDestroyProcessStates) for correct deallocation.
//   - Free() is the sole deallocation path. It recovers alignment
//     from the header and calls operator delete with matching align_val.
class AlignedBuffer {
  static constexpr size_t kTagSize = sizeof(uint64_t);

  struct Deleter {
    std::align_val_t alignment;
    void operator()(char* base) const {
      ::operator delete(base, alignment);
    }
  };

  // NOLINTBEGIN(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  // Low-level allocator metadata recovery: navigating backward from
  // data pointer to alignment tag and base requires pointer arithmetic
  // with no span/container alternative.
  static auto RecoverAlignment(const void* data_ptr) -> uint64_t {
    uint64_t alignment = 0;
    const auto* data = static_cast<const std::byte*>(data_ptr);
    std::memcpy(&alignment, data - kTagSize, kTagSize);
    return alignment;
  }

  static auto RecoverBasePointer(void* data_ptr, uint64_t alignment) -> void* {
    auto* data = static_cast<std::byte*>(data_ptr);
    return static_cast<void*>(data - alignment);
  }
  // NOLINTEND(cppcoreguidelines-pro-bounds-pointer-arithmetic)

  std::unique_ptr<char, Deleter> base_;
  char* data_ = nullptr;

 public:
  AlignedBuffer(uint64_t alloc_align, uint64_t data_size) {
    uint64_t total = CheckedAdd(alloc_align, data_size);
    auto align_val = std::align_val_t{alloc_align};
    auto* raw =
        static_cast<char*>(::operator new(total, align_val, std::nothrow));
    if (raw == nullptr) {
      throw common::InternalError(
          "LyraConstructProcessStates", "allocation failed");
    }
    base_ = std::unique_ptr<char, Deleter>(raw, Deleter{align_val});
    // Header occupies [base, base+alloc_align). Data starts after.
    auto block = std::span(raw, total);
    data_ = block.subspan(alloc_align).data();
    // Store alignment in last kTagSize bytes of header for recovery.
    auto tag = block.subspan(alloc_align - kTagSize, kTagSize);
    std::memcpy(tag.data(), &alloc_align, kTagSize);
  }

  auto Data() -> char* {
    return data_;
  }

  // Transfer ownership to the caller. The caller must pass the returned
  // pointer to Free() (via LyraDestroyProcessStates) for deallocation.
  auto Release() -> char* {
    base_.release();
    auto* p = data_;
    data_ = nullptr;
    return p;
  }

  // Recover alignment from header and deallocate. Accepts nullptr (no-op).
  // This is the sole deallocation path for buffers created by this class.
  static void Free(void* data_ptr) {
    if (data_ptr == nullptr) {
      return;
    }
    uint64_t alignment = RecoverAlignment(data_ptr);
    void* base = RecoverBasePointer(data_ptr, alignment);
    ::operator delete(base, std::align_val_t{alignment});
  }
};

}  // namespace

}  // namespace lyra::runtime

extern "C" auto LyraConstructProcessStates(
    void** states_out_raw, uint32_t num_processes,
    const lyra::runtime::ProcessStateSchema* state_schemas_raw,
    uint32_t num_state_schemas,
    const lyra::runtime::ProcessConstructorRecord* records_raw) -> void* {
  using lyra::common::InternalError;
  using lyra::runtime::AlignedBuffer;
  using lyra::runtime::CheckedAdd;
  using lyra::runtime::CheckedAlignUp;
  using lyra::runtime::IsPowerOfTwo;
  using lyra::runtime::kMinBaseAlignment;

  if (num_processes == 0) {
    return nullptr;
  }

  if (states_out_raw == nullptr) {
    throw InternalError("LyraConstructProcessStates", "states_out is null");
  }
  if (state_schemas_raw == nullptr) {
    throw InternalError("LyraConstructProcessStates", "state_schemas is null");
  }
  if (records_raw == nullptr) {
    throw InternalError("LyraConstructProcessStates", "records is null");
  }

  auto records = std::span(records_raw, num_processes);
  auto schemas = std::span(state_schemas_raw, num_state_schemas);
  auto states_out = std::span(states_out_raw, num_processes);

  // Pass 1: validate and compute total packed buffer size.
  // Packing uses exact schema alignments -- no promotion.
  uint64_t total_size = 0;
  uint64_t max_schema_align = 1;

  for (const auto& rec : records) {
    if (rec.schema_index >= num_state_schemas) {
      throw InternalError(
          "LyraConstructProcessStates", "schema_index out of range");
    }

    const auto& schema = schemas[rec.schema_index];
    if (!IsPowerOfTwo(schema.state_align)) {
      throw InternalError("LyraConstructProcessStates", "invalid state_align");
    }

    max_schema_align = std::max(max_schema_align, schema.state_align);
    total_size = CheckedAlignUp(total_size, schema.state_align);
    total_size = CheckedAdd(total_size, schema.state_size);
  }

  // The allocation base alignment is the maximum of all schema alignments
  // and the allocator's minimum. This only affects the buffer's base
  // address, not the packed layout within it.
  uint64_t alloc_align = std::max(max_schema_align, kMinBaseAlignment);
  uint64_t alloc_size =
      CheckedAlignUp(std::max(total_size, uint64_t{1}), alloc_align);

  // Pass 2: allocate and zero.
  AlignedBuffer buf(alloc_align, alloc_size);
  auto buffer = std::span(buf.Data(), alloc_size);
  std::memset(buffer.data(), 0, alloc_size);

  // Pass 3: distribute state pointers and call frame_init.
  // Uses exact schema alignments, mirroring Pass 1.
  uint64_t offset = 0;
  for (uint32_t i = 0; i < num_processes; ++i) {
    const auto& schema = schemas[records[i].schema_index];
    offset = CheckedAlignUp(offset, schema.state_align);
    uint64_t end = CheckedAdd(offset, schema.state_size);
    if (end > alloc_size) {
      throw InternalError(
          "LyraConstructProcessStates",
          "state offset exceeds allocated buffer");
    }
    states_out[i] = buffer.subspan(offset).data();
    if (schema.frame_init != nullptr) {
      schema.frame_init(states_out[i]);
    }
    offset = end;
  }

  if (offset != total_size) {
    throw InternalError(
        "LyraConstructProcessStates",
        "offset/size mismatch after distribution");
  }

  return buf.Release();
}

extern "C" void LyraDestroyProcessStates(void* packed_buffer) {
  lyra::runtime::AlignedBuffer::Free(packed_buffer);
}
