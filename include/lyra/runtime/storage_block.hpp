#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/scope_program.hpp"

namespace lyra::runtime {

// The storage a body's declared variables live in for one activation: one
// storage object per descriptor, in schema order, reached by the index the
// declaration gave that slot.
class StorageBlock {
 public:
  explicit StorageBlock(MemberStorageSchema schema);

  // Where slot `index` lives, which is what a place naming it resolves to.
  [[nodiscard]] auto Address(std::uint32_t index) -> void*;

 private:
  std::vector<std::unique_ptr<MemberStorage>> slots_;
};

}  // namespace lyra::runtime
