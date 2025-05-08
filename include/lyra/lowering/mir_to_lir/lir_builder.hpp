#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lyra/lir/basic_block.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::lowering {

class LirBuilder {
 public:
  explicit LirBuilder(std::string module_name);

  void AddVariable(const common::Variable& variable);
  void BeginProcess(lir::ProcessKind kind, const std::string& name);

  // Basic block management
  void StartBlock(const std::string& label);
  void EndBlock();
  auto NewLabel(const std::string& prefix = "label") -> std::string;

  void AddInstruction(lir::Instruction instruction);

  void EndProcess();

  auto MakeTemp(const std::string& hint = "tmp") -> std::string;
  auto Build() -> std::unique_ptr<lir::Module>;

 private:
  std::string module_name_;
  std::vector<common::Variable> variables_;
  std::vector<std::shared_ptr<lir::Process>> processes_;
  std::shared_ptr<lir::Process> current_process_;

  // Basic block management
  std::unique_ptr<lir::BasicBlock> current_block_;
  std::vector<std::unique_ptr<lir::BasicBlock>> current_blocks_;
  int label_counter_ = 0;

  int temp_counter_ = 0;
};

}  // namespace lyra::lowering
