#pragma once

#include "core/execution_context.hpp"
#include "lir/module.hpp"

namespace volans::lir {

class Executor {
 public:
  Executor(const Module& module, volans::ExecutionContext& context);

  void RunInitial();

 private:
  std::reference_wrapper<const Module> module_;
  std::reference_wrapper<volans::ExecutionContext> ctx_;
};

}  // namespace volans::lir
