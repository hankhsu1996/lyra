#include "lyra/runtime/loop_blocks.hpp"

#include <cstdint>
#include <format>

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

LoopBlocks::LoopBlocks() = default;
LoopBlocks::~LoopBlocks() = default;

void LoopBlocks::Add(std::int64_t index, Scope* block) {
  if (!blocks_.emplace(index, block).second) {
    throw InternalError(
        std::format(
            "LoopBlocks::Add: a second block was built where the index stood "
            "at {}",
            index));
  }
}

auto LoopBlocks::At(std::int64_t index) const -> Scope* {
  const auto found = blocks_.find(index);
  if (found == blocks_.end()) {
    throw InternalError(
        std::format(
            "LoopBlocks::At: a name was resolved to a block at {} that this "
            "loop did not build",
            index));
  }
  return found->second;
}

}  // namespace lyra::runtime
