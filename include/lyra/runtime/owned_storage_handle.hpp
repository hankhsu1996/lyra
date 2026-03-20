#pragma once

#include <cstddef>
#include <cstdint>

namespace lyra::runtime {

// Runtime representation of owned backing storage for a design-global slot.
// Stored inline in the design-state inline region at a fixed offset.
// The backing data lives in the arena's appendix region.
//
// This is a generic ownership handle. Array element count and stride
// are derived from the slot's compile-time storage spec, not from
// the handle. The handle.data field is strictly an arena-internal
// backing base pointer, not a typed element pointer.
struct OwnedStorageHandle {
  void* data;          // Pointer to backing storage (arena-internal)
  uint64_t byte_size;  // Total backing bytes
};

static_assert(sizeof(OwnedStorageHandle) == 16);
static_assert(alignof(OwnedStorageHandle) == 8);
static_assert(offsetof(OwnedStorageHandle, data) == 0);
static_assert(offsetof(OwnedStorageHandle, byte_size) == 8);

// Inline byte size and alignment of OwnedStorageHandle in the design-state
// inline region. Used by layout computation.
inline constexpr uint64_t kOwnedStorageHandleByteSize = 16;
inline constexpr uint64_t kOwnedStorageHandleAlignment = 8;

}  // namespace lyra::runtime
