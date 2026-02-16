#pragma once

#include <memory>
#include <vector>

#include "lyra/common/assoc_array_handle.hpp"

namespace lyra::runtime {

// Forward declarations
struct KeySpec;
class AssocArrayObj;

class AssocArrayHeap {
 public:
  AssocArrayHeap();
  ~AssocArrayHeap();
  AssocArrayHeap(AssocArrayHeap&&) noexcept;
  auto operator=(AssocArrayHeap&&) noexcept -> AssocArrayHeap&;

  auto Allocate(KeySpec spec) -> AssocArrayHandle;
  auto Get(AssocArrayHandle h) -> AssocArrayObj&;
  auto Get(AssocArrayHandle h) const -> const AssocArrayObj&;
  auto DeepCopy(AssocArrayHandle src) -> AssocArrayHandle;

 private:
  std::vector<std::unique_ptr<AssocArrayObj>> objects_;
};

}  // namespace lyra::runtime
