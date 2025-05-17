#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lyra/lir/basic_block.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::lowering {

class LirBuilder {
 public:
  explicit LirBuilder(
      std::string module_name, std::shared_ptr<lir::LirContext> context);

  // Module interface
  auto Build() -> std::unique_ptr<lir::Module>;
  void AddVariable(const common::Variable& variable);

  // Process interface
  void BeginProcess(lir::ProcessKind kind, const std::string& name);
  void EndProcess();

  // Basic block interface
  void StartBlock(lir::LabelRef label);
  void EndBlock();

  // Instruction interface
  void AddInstruction(lir::Instruction instruction);

  // Allocation helpers
  auto AllocateTemp(const std::string& hint, common::Type type) -> lir::TempRef;
  auto MakeLabel(const std::string& hint) -> lir::LabelRef;
  auto InternLiteral(const common::Literal& literal) -> lir::LiteralRef;

 private:
  auto InternLabel(const std::string& name) -> lir::LabelRef;

  std::string module_name_;
  std::vector<common::Variable> variables_;
  std::vector<std::shared_ptr<lir::Process>> processes_;
  std::shared_ptr<lir::Process> current_process_;

  std::unique_ptr<lir::BasicBlock> current_block_;
  std::vector<std::unique_ptr<lir::BasicBlock>> current_blocks_;

  int label_counter_ = 0;
  int temp_counter_ = 0;

  std::shared_ptr<lir::LirContext> context_;
};

}  // namespace lyra::lowering
