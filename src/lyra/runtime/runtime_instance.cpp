#include "lyra/runtime/runtime_instance.hpp"

#include <cstring>
#include <format>
#include <new>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

auto AllocateOwnedInlineStorage(uint64_t size) -> std::byte* {
  if (size == 0) return nullptr;
  auto* mem = new (std::nothrow) std::byte[size];
  if (mem == nullptr) {
    throw common::InternalError(
        "AllocateOwnedInlineStorage",
        std::format("allocation of {} bytes failed", size));
  }
  std::memset(mem, 0, size);
  return mem;
}

auto AllocateOwnedAppendixStorage(uint64_t size) -> std::byte* {
  if (size == 0) return nullptr;
  auto* mem = new (std::nothrow) std::byte[size];
  if (mem == nullptr) {
    throw common::InternalError(
        "AllocateOwnedAppendixStorage",
        std::format("allocation of {} bytes failed", size));
  }
  std::memset(mem, 0, size);
  return mem;
}

void FreeRuntimeInstanceStorage(RuntimeInstanceStorage& storage) {
  delete[] storage.inline_base;
  storage.inline_base = nullptr;
  delete[] storage.appendix_base;
  storage.appendix_base = nullptr;
}

}  // namespace lyra::runtime
