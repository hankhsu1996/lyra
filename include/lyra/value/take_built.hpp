#pragma once

#include <array>
#include <bit>
#include <cstddef>
#include <memory>
#include <new>
#include <utility>

namespace lyra::value {

// The value of `T` that `build` lays out in the storage it is handed, taken out
// of that storage: how code holding a `T` reads what an operation answers in
// storage it was given.
template <typename T, typename Build>
[[nodiscard]] auto TakeBuilt(Build build) -> T {
  alignas(T) std::array<std::byte, sizeof(T)> storage{};
  build(static_cast<void*>(storage.data()));
  T* built = std::launder(std::bit_cast<T*>(storage.data()));
  T answer = std::move(*built);
  std::destroy_at(built);
  return answer;
}

}  // namespace lyra::value
