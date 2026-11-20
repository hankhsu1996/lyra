#pragma once

#include <memory>
#include <utility>

namespace lyra::value {

// The value `storage` holds, taken out of it: the storage is left holding no
// value, as Rust's `ptr::read` leaves it. A C++ value moved from still has to
// be ended, so the value is moved into the answer and the storage then ended.
template <typename T>
[[nodiscard]] auto PtrRead(T* storage) -> T {
  T taken(std::move(*storage));
  std::destroy_at(storage);
  return taken;
}

// Relocates the value `value` points at into the storage `out`, answering
// that storage, which holds the value from then on; nothing of the storage
// the value left is owed an end afterwards. A C++ object is moved and the
// object moved from destroyed, since C++ gives a host compiler no relocation
// it is bound to provide.
template <typename T>
auto Relocate(void* value, void* out) noexcept -> void* {
  T* const source = static_cast<T*>(value);
  T* const built = std::construct_at(static_cast<T*>(out), std::move(*source));
  std::destroy_at(source);
  return built;
}

}  // namespace lyra::value
