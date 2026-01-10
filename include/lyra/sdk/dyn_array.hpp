#pragma once

#include <algorithm>
#include <cstddef>
#include <vector>

namespace lyra::sdk {

// Resize a dynamic array while copying elements from an initializer array.
// If new_size > init.size(), pad with default-constructed elements.
// If new_size < init.size(), truncate.
// Used for SystemVerilog: arr = new[size](init)
template <typename T>
auto DynArrayResize(size_t new_size, const std::vector<T>& init)
    -> std::vector<T> {
  std::vector<T> result(new_size);
  size_t copy_count = std::min(new_size, init.size());
  std::copy_n(init.begin(), copy_count, result.begin());
  return result;
}

}  // namespace lyra::sdk
