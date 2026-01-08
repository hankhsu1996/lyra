#include "lyra/lowering/mir_to_lir/lir_builder.hpp"

#include <cassert>
#include <cstddef>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/internal_error.hpp"
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
  module_->processes = std::move(processes_);
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
  assert(!current_process_);
  current_process_ = std::make_shared<lir::Process>();
  current_process_->name = name;
  current_blocks_.clear();
}

void LirBuilder::EndProcess() {
  assert(current_process_);

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
}

void LirBuilder::AddProcessVariable(const common::Variable& variable) {
  assert(current_process_);
  current_process_->variables.push_back(variable);
}

void LirBuilder::StartBlock(lir::LabelRef label) {
  assert(current_process_);

  if (current_block_) {
    EndBlock();
  }

  current_block_ = std::make_unique<lir::BasicBlock>();
  current_block_->label = label;
}

void LirBuilder::EndBlock() {
  assert(current_process_);
  assert(current_block_);

  current_blocks_.push_back(std::move(current_block_));
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

auto LirBuilder::InternLabel(const std::string& name) -> lir::LabelRef {
  return context_->InternLabel(name);
}

auto LirBuilder::InternLiteral(const common::Literal& literal)
    -> lir::LiteralRef {
  return context_->InternLiteral(literal);
}

namespace {

/// Validates that a MonitorExpressionBlock contains no side effects.
/// Expression blocks are re-evaluated at each time slot, so they must be pure.
void ValidateMonitorExpressionBlock(const lir::MonitorExpressionBlock& block) {
  using IK = lir::InstructionKind;

  for (const auto& instr : block.instructions) {
    switch (instr.kind) {
      case IK::kStoreVariable:
      case IK::kStoreVariableNonBlocking:
      case IK::kStoreUnpackedElement:
      case IK::kStorePackedElement:
        throw common::InternalError(
            "AddMonitorExpressionBlock",
            "expression block contains store instruction - expressions must be "
            "side-effect free");

      case IK::kSystemCall:
        // System calls are not allowed in expression blocks
        throw common::InternalError(
            "AddMonitorExpressionBlock",
            fmt::format(
                "expression block contains system call '{}' - expressions must "
                "be side-effect free",
                instr.system_call_name));

      default:
        // All other instructions are allowed (loads, literals, arithmetic,
        // etc.)
        break;
    }
  }
}

}  // namespace

auto LirBuilder::AddMonitorExpressionBlock(lir::MonitorExpressionBlock block)
    -> size_t {
  assert(module_);
  ValidateMonitorExpressionBlock(block);
  size_t index = module_->monitor_expression_blocks.size();
  module_->monitor_expression_blocks.push_back(std::move(block));
  return index;
}

auto LirBuilder::GetCurrentBlockInstructionCount() const -> size_t {
  assert(current_block_);
  return current_block_->instructions.size();
}

auto LirBuilder::CopyInstructionsSince(size_t start_index) const
    -> std::vector<lir::Instruction> {
  assert(current_block_);
  auto& instrs = current_block_->instructions;
  assert(start_index <= instrs.size());
  return {
      instrs.begin() + static_cast<std::ptrdiff_t>(start_index), instrs.end()};
}

}  // namespace lyra::lowering::mir_to_lir
