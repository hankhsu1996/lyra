#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lir/instruction.hpp"
#include "lir/module.hpp"
#include "lir/process.hpp"
#include "lir/value.hpp"

namespace lyra::lowering {

class LirBuilder {
 public:
  explicit LirBuilder(std::string module_name);

  void AddSignal(const std::string& name);
  void BeginProcess(lir::ProcessKind kind);
  void AddInstruction(
      lir::InstructionKind kind, const std::string& result,
      std::vector<lir::Value> operands);
  void EndProcess();

  auto MakeTemp(const std::string& hint = "tmp") -> std::string;
  auto Build() -> lir::Module;

 private:
  std::string module_name_;
  std::vector<std::string> signals_;
  std::vector<std::shared_ptr<lir::Process>> processes_;
  std::shared_ptr<lir::Process> current_process_;
  int temp_counter_ = 0;
};

}  // namespace lyra::lowering
