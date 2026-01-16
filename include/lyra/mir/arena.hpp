#pragma once

#include <vector>

#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::mir {

class Arena final {
 public:
  Arena() = default;
  ~Arena() = default;

  Arena(const Arena&) = delete;
  auto operator=(const Arena&) -> Arena& = delete;

  Arena(Arena&&) = default;
  auto operator=(Arena&&) -> Arena& = default;

  auto AddPlace(Place place) -> PlaceId {
    PlaceId id{static_cast<uint32_t>(places_.size())};
    places_.push_back(std::move(place));
    return id;
  }

  auto AddBasicBlock(BasicBlock block) -> BasicBlockId {
    BasicBlockId id{static_cast<uint32_t>(basic_blocks_.size())};
    basic_blocks_.push_back(std::move(block));
    return id;
  }

  auto AddProcess(Process proc) -> ProcessId {
    ProcessId id{static_cast<uint32_t>(processes_.size())};
    processes_.push_back(std::move(proc));
    return id;
  }

  auto AddFunction(Function func) -> FunctionId {
    FunctionId id{static_cast<uint32_t>(functions_.size())};
    functions_.push_back(std::move(func));
    return id;
  }

  [[nodiscard]] auto operator[](PlaceId id) const -> const Place& {
    return places_[id.value];
  }

  [[nodiscard]] auto operator[](BasicBlockId id) const -> const BasicBlock& {
    return basic_blocks_[id.value];
  }

  // Update a basic block that was previously added via AddBasicBlock.
  // This is ONLY for use during MIR construction (in MirBuilder).
  // Do not call after lowering is complete - MIR should be immutable
  // post-construction.
  void UpdateBasicBlock(BasicBlockId id, BasicBlock block) {
    basic_blocks_[id.value] = std::move(block);
  }

  [[nodiscard]] auto operator[](ProcessId id) const -> const Process& {
    return processes_[id.value];
  }

  [[nodiscard]] auto operator[](FunctionId id) const -> const Function& {
    return functions_[id.value];
  }

 private:
  std::vector<Place> places_;
  std::vector<BasicBlock> basic_blocks_;
  std::vector<Process> processes_;
  std::vector<Function> functions_;
};

}  // namespace lyra::mir
