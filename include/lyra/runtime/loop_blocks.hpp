#pragma once

#include <cstdint>
#include <unordered_map>

namespace lyra::runtime {

class Scope;

// The blocks one loop generate built in one object (LRM 27.4), each under the
// value its index stood at when it was built. Which values those are is what
// the object's own construction counted out, so only the object knows where a
// block stands, and a hierarchical name reaching one asks for it by that value
// (LRM 23.6) -- the name states what the language calls the block, and this
// answers where it is.
class LoopBlocks {
 public:
  // Defined in the library: whatever holds a loop constructs and destroys one,
  // and a definition written here would be compiled again by every unit that
  // does.
  LoopBlocks();
  LoopBlocks(const LoopBlocks&) = delete;
  auto operator=(const LoopBlocks&) -> LoopBlocks& = delete;
  LoopBlocks(LoopBlocks&&) = delete;
  auto operator=(LoopBlocks&&) -> LoopBlocks& = delete;
  ~LoopBlocks();

  // Files the block built where the index stood at `index`. A loop builds at
  // most one block at any value of its index (LRM 27.4).
  void Add(std::int64_t index, Scope* block);

  // The block built where the index stood at `index`. The front end resolved
  // the name to a block every object that resolution covered built, so one
  // stands there.
  [[nodiscard]] auto At(std::int64_t index) const -> Scope*;

 private:
  std::unordered_map<std::int64_t, Scope*> blocks_;
};

}  // namespace lyra::runtime
