#pragma once

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

class OutputDispatcher;

class RuntimeServices {
 public:
  explicit RuntimeServices(OutputDispatcher& output) : output_(&output) {
  }

  auto Output() -> OutputDispatcher& {
    if (output_ == nullptr) {
      throw InternalError("RuntimeServices has no OutputDispatcher");
    }
    return *output_;
  }

 private:
  OutputDispatcher* output_ = nullptr;
};

}  // namespace lyra::runtime
