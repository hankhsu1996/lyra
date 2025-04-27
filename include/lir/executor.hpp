#pragma once

#include "core/execution_context.hpp"
#include "lir/module.hpp"

namespace lyra::lir {

class Executor {
 public:
  Executor(const Module& module, lyra::ExecutionContext& context);

  void RunInitial();

 private:
  std::reference_wrapper<const Module> module_;
  std::reference_wrapper<lyra::ExecutionContext> ctx_;
};

}  // namespace lyra::lir
