#include "lyra/runtime/runtime_instance.hpp"

#include <cstring>
#include <format>
#include <new>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

// RuntimeInstanceStorage owns raw heap byte regions whose layout is a binary
// contract with codegen (accessed via GEP). Raw new[]/delete[] is intentional;
// unique_ptr would change the struct layout.
// NOLINTBEGIN(cppcoreguidelines-owning-memory)

auto AllocateOwnedStorage(uint64_t size, const char* caller) -> std::byte* {
  if (size == 0) return nullptr;
  auto* mem = new (std::nothrow) std::byte[size];
  if (mem == nullptr) {
    throw common::InternalError(
        caller, std::format("allocation of {} bytes failed", size));
  }
  std::memset(mem, 0, size);
  return mem;
}

auto AllocateOwnedInlineStorage(uint64_t size) -> std::byte* {
  return AllocateOwnedStorage(size, "AllocateOwnedInlineStorage");
}

auto AllocateOwnedAppendixStorage(uint64_t size) -> std::byte* {
  return AllocateOwnedStorage(size, "AllocateOwnedAppendixStorage");
}

void FreeRuntimeInstanceStorage(RuntimeInstanceStorage& storage) {
  delete[] storage.inline_base;
  storage.inline_base = nullptr;
  delete[] storage.appendix_base;
  storage.appendix_base = nullptr;
  delete[] storage.deferred_inline_base;
  storage.deferred_inline_base = nullptr;
}

// NOLINTEND(cppcoreguidelines-owning-memory)

}  // namespace lyra::runtime
