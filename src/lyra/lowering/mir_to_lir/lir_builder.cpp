#include "lyra/lowering/mir_to_lir/lir_builder.hpp"

#include <cassert>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/lir/basic_block.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::lowering::mir_to_lir {

LirBuilder::LirBuilder(
    std::string module_name, std::shared_ptr<lir::LirContext> context)
    : module_name_(std::move(module_name)), context_(std::move(context)) {
}

void LirBuilder::BeginModule() {
  module_ = std::make_unique<lir::Module>();
  module_->name = module_name_;
  module_->context = context_;
}

auto LirBuilder::EndModule() -> std::unique_ptr<lir::Module> {
  assert(module_);
  // Append regular processes to any synthetic processes already added
  for (auto& process : processes_) {
    module_->processes.push_back(std::move(process));
  }
  processes_.clear();
  return std::move(module_);
}

void LirBuilder::AddModuleVariable(const common::Variable& variable) {
  assert(module_);
  module_->variables.push_back(variable);
}

void LirBuilder::AddPort(
    const common::Variable& variable, lir::PortDirection direction) {
  assert(module_);
  module_->ports.push_back(
      lir::Port{.variable = variable, .direction = direction});
}

void LirBuilder::AddSubmodule(const lir::SubmoduleInstance& submodule) {
  assert(module_);
  module_->submodules.push_back(submodule);
}

void LirBuilder::BeginProcess(const std::string& name) {
  assert(procedural_context_ == ProceduralContext::kModule);
  procedural_context_ = ProceduralContext::kProcess;
  current_process_ = std::make_shared<lir::Process>();
  current_process_->name = name;
  current_blocks_.clear();
}

void LirBuilder::EndProcess() {
  assert(procedural_context_ == ProceduralContext::kProcess);

  AddInstruction(lir::Instruction::Complete());

  if (current_block_) {
    EndBlock();
  }

  if (!current_blocks_.empty()) {
    current_process_->blocks.reserve(current_blocks_.size());
    for (auto& block : current_blocks_) {
      current_process_->blocks.push_back(std::move(block));
    }
    current_blocks_.clear();
  }

  processes_.push_back(std::move(current_process_));
  current_process_ = nullptr;
  procedural_context_ = ProceduralContext::kModule;
}

void LirBuilder::RegisterLocalVariable(const common::Variable& variable) {
  switch (procedural_context_) {
    case ProceduralContext::kProcess:
      assert(current_process_);
      current_process_->variables.push_back(variable);
      break;
    case ProceduralContext::kFunction:
      // Function local variables are pre-registered in MIR's
      // FunctionDefinition.local_variables, so nothing to do here.
      break;
    case ProceduralContext::kModule:
      assert(false && "variable declaration outside procedural context");
      break;
  }
}

void LirBuilder::BeginFunction(const std::string& /*name*/) {
  assert(procedural_context_ == ProceduralContext::kModule);
  procedural_context_ = ProceduralContext::kFunction;
  function_blocks_.clear();
}

void LirBuilder::EndFunction() {
  assert(procedural_context_ == ProceduralContext::kFunction);

  // End the current block if any (EndBlock handles adding implicit returns)
  if (current_block_) {
    EndBlock();
  }

  procedural_context_ = ProceduralContext::kModule;
}

auto LirBuilder::TakeFunctionBlocks()
    -> std::vector<std::unique_ptr<lir::BasicBlock>> {
  return std::move(function_blocks_);
}

void LirBuilder::BeginSyntheticFunction(const std::string& name) {
  // Must be in a process context to nest a synthetic function
  assert(procedural_context_ == ProceduralContext::kProcess);
  assert(!saved_process_state_.has_value());

  // Save current process state
  saved_process_state_ = SavedProcessState{
      .context = procedural_context_,
      .process = std::move(current_process_),
      .block = std::move(current_block_),
      .blocks = std::move(current_blocks_),
  };

  // Switch to function context
  procedural_context_ = ProceduralContext::kFunction;
  function_blocks_.clear();
  synthetic_function_name_ = name;
}

auto LirBuilder::EndSyntheticFunction() -> std::string {
  assert(procedural_context_ == ProceduralContext::kFunction);
  assert(saved_process_state_.has_value());

  // End the current block if any
  if (current_block_) {
    EndBlock();
  }

  // Create the function and add to module
  lir::Function func;
  func.name = synthetic_function_name_;
  func.return_type = common::Type::Void();
  func.blocks = std::move(function_blocks_);
  if (!func.blocks.empty()) {
    func.entry_label = func.blocks[0]->label;
  }
  module_->functions.push_back(std::move(func));

  // Restore process state
  procedural_context_ = saved_process_state_->context;
  current_process_ = std::move(saved_process_state_->process);
  current_block_ = std::move(saved_process_state_->block);
  current_blocks_ = std::move(saved_process_state_->blocks);
  saved_process_state_.reset();

  return synthetic_function_name_;
}

void LirBuilder::BeginSyntheticProcess(const std::string& name) {
  assert(procedural_context_ == ProceduralContext::kProcess);
  assert(!saved_process_state_.has_value());

  // Save current process state
  saved_process_state_ = SavedProcessState{
      .context = procedural_context_,
      .process = std::move(current_process_),
      .block = std::move(current_block_),
      .blocks = std::move(current_blocks_),
  };

  // Start new process (stay in kProcess context)
  // Mark as callback so it won't be auto-scheduled
  current_process_ = std::make_shared<lir::Process>();
  current_process_->name = name;
  current_process_->is_callback = true;
  current_blocks_.clear();
  synthetic_process_name_ = name;
}

auto LirBuilder::EndSyntheticProcess() -> std::string {
  assert(procedural_context_ == ProceduralContext::kProcess);
  assert(saved_process_state_.has_value());

  // End current block if any (don't auto-add terminators - caller must add)
  if (current_block_) {
    current_blocks_.push_back(std::move(current_block_));
    current_block_ = nullptr;
  }

  // Finalize process and add directly to module
  current_process_->blocks.reserve(current_blocks_.size());
  for (auto& block : current_blocks_) {
    current_process_->blocks.push_back(std::move(block));
  }
  module_->processes.push_back(std::move(current_process_));

  // Restore original process state
  procedural_context_ = saved_process_state_->context;
  current_process_ = std::move(saved_process_state_->process);
  current_block_ = std::move(saved_process_state_->block);
  current_blocks_ = std::move(saved_process_state_->blocks);
  saved_process_state_.reset();

  return synthetic_process_name_;
}

void LirBuilder::StartBlock(lir::LabelRef label) {
  // Must be in a procedural context (process or function)
  assert(procedural_context_ != ProceduralContext::kModule);

  if (current_block_) {
    EndBlock();
  }

  current_block_ = std::make_unique<lir::BasicBlock>();
  current_block_->label = label;
}

void LirBuilder::EndBlock() {
  // Must be in a procedural context (process or function)
  assert(procedural_context_ != ProceduralContext::kModule);
  assert(current_block_);

  // For function blocks, ensure they end with a terminator
  if (procedural_context_ == ProceduralContext::kFunction) {
    bool needs_return = true;
    if (!current_block_->instructions.empty()) {
      auto last_kind = current_block_->instructions.back().kind;
      if (last_kind == lir::InstructionKind::kReturn ||
          last_kind == lir::InstructionKind::kJump ||
          last_kind == lir::InstructionKind::kBranch) {
        needs_return = false;
      }
    }
    if (needs_return) {
      current_block_->instructions.push_back(lir::Instruction::Return());
    }
    function_blocks_.push_back(std::move(current_block_));
  } else {
    current_blocks_.push_back(std::move(current_block_));
  }
  current_block_ = nullptr;
}

void LirBuilder::AddInstruction(lir::Instruction instr) {
  assert(current_block_);
  current_block_->instructions.push_back(std::move(instr));
}

auto LirBuilder::AllocateTemp(const std::string& hint, common::Type type)
    -> lir::TempRef {
  std::string name = fmt::format("%{}_{}", hint, temp_counter_++);
  return context_->AllocateTemp(name, type);
}

auto LirBuilder::MakeLabel(const std::string& hint) -> lir::LabelRef {
  std::string name = hint + "." + std::to_string(label_counter_++);
  return InternLabel(name);
}

auto LirBuilder::MakeSyntheticFunctionName(const std::string& hint)
    -> std::string {
  return "__" + hint + "_" + std::to_string(synthetic_function_counter_++);
}

auto LirBuilder::InternLabel(const std::string& name) -> lir::LabelRef {
  return context_->InternLabel(name);
}

auto LirBuilder::InternLiteral(const common::Literal& literal)
    -> lir::LiteralRef {
  return context_->InternLiteral(literal);
}

}  // namespace lyra::lowering::mir_to_lir
