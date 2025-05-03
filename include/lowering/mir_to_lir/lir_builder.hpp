#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lir/basic_block.hpp"
#include "lir/instruction.hpp"
#include "lir/module.hpp"
#include "lir/process.hpp"
#include "lir/value.hpp"

namespace lyra::lowering {

class LirBuilder {
 public:
  explicit LirBuilder(std::string module_name);

  void AddVariable(const std::string& name);
  void BeginProcess(lir::ProcessKind kind);
  void AddTrigger(lir::Trigger trigger);

  // Basic block management
  void StartBlock(const std::string& label);
  void EndBlock();
  auto NewLabel(const std::string& prefix = "label") -> std::string;

  // Instruction addition
  void AddInstruction(
      lir::InstructionKind kind, const std::string& result,
      std::vector<lir::Value> operands);
  void AddInstruction(
      lir::InstructionKind kind, const std::string& result,
      std::vector<lir::Value> operands, const std::string& system_call_name);

  void EndProcess();

  auto MakeTemp(const std::string& hint = "tmp") -> std::string;
  auto Build() -> std::unique_ptr<lir::Module>;

 private:
  std::string module_name_;
  std::vector<std::string> variables_;
  std::vector<std::shared_ptr<lir::Process>> processes_;
  std::shared_ptr<lir::Process> current_process_;

  // Basic block management
  std::unique_ptr<lir::BasicBlock> current_block_;
  std::vector<std::unique_ptr<lir::BasicBlock>> current_blocks_;
  int label_counter_ = 0;

  int temp_counter_ = 0;
};

}  // namespace lyra::lowering
