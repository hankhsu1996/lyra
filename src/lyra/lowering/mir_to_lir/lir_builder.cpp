#include "lyra/lowering/mir_to_lir/lir_builder.hpp"

namespace lyra::lowering {

LirBuilder::LirBuilder(std::string module_name)
    : module_name_(std::move(module_name)) {
}

void LirBuilder::AddVariable(const common::Variable& variable) {
  variables_.push_back(variable);
}

void LirBuilder::BeginProcess(lir::ProcessKind kind, const std::string& name) {
  assert(!current_process_);
  current_process_ = std::make_shared<lir::Process>();
  current_process_->kind = kind;
  current_process_->name = name;
  current_blocks_.clear();
}

void LirBuilder::StartBlock(const std::string& label) {
  assert(current_process_);

  // If we have an active block, end it before starting a new one
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

auto LirBuilder::NewLabel(const std::string& prefix) -> std::string {
  return prefix + "_" + std::to_string(label_counter_++);
}

void LirBuilder::AddInstruction(lir::Instruction instr) {
  assert(current_block_);
  current_block_->instructions.push_back(std::move(instr));
}

void LirBuilder::EndProcess() {
  assert(current_process_);

  // Insert 'complete' instruction for initial process
  if (current_process_->kind == lir::ProcessKind::kInitial) {
    AddInstruction(lir::Instruction::Complete());
  }

  // If we have an active block, end it
  if (current_block_) {
    EndBlock();
  }

  // If we have blocks, move them to the process
  if (!current_blocks_.empty()) {
    // Convert vector of unique_ptr to vector of unique_ptr, moving ownership
    current_process_->blocks.reserve(current_blocks_.size());
    for (auto& block : current_blocks_) {
      current_process_->blocks.push_back(std::move(block));
    }
    current_blocks_.clear();
  }

  processes_.push_back(std::move(current_process_));
  current_process_ = nullptr;
}

auto LirBuilder::MakeTemp(const std::string& hint) -> std::string {
  if (hint.empty()) {
    return "%" + std::to_string(temp_counter_++);
  }
  return "%" + hint + "_" + std::to_string(temp_counter_++);
}

auto LirBuilder::Build() -> std::unique_ptr<lir::Module> {
  auto mod = std::make_unique<lir::Module>();
  mod->name = module_name_;
  mod->variables = std::move(variables_);
  mod->processes = std::move(processes_);
  return mod;
}

}  // namespace lyra::lowering
