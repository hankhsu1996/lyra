// context.hpp

#pragma once

#include <vector>

#include "lyra/lir/context.hpp"

namespace lyra::lowering::mir_to_lir {

class LoweringContext {
 public:
  struct Loop {
    lir::LabelRef continue_label;
    lir::LabelRef break_label;
  };

  void PushLoop(Loop loop) {
    loop_stack_.push_back(std::move(loop));
  }

  void PopLoop() {
    loop_stack_.pop_back();
  }

  [[nodiscard]] auto CurrentLoop() const -> const Loop& {
    return loop_stack_.back();
  }

  [[nodiscard]] auto HasLoop() const -> bool {
    return !loop_stack_.empty();
  }

 private:
  std::vector<Loop> loop_stack_;
};

}  // namespace lyra::lowering::mir_to_lir
