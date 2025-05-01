#include "lowering/mir_to_lir/lir_builder.hpp"

namespace lyra::lowering {

LirBuilder::LirBuilder(std::string module_name)
    : module_name_(std::move(module_name)) {
}

void LirBuilder::AddSignal(const std::string& name) {
  signals_.push_back(name);
}

void LirBuilder::BeginProcess(lir::ProcessKind kind) {
  if (current_process_) {
    throw std::runtime_error(
        "BeginProcess called while a process is already active");
  }

  current_process_ = std::make_shared<lir::Process>();
  current_process_->kind = kind;
  current_blocks_.clear();
}

void LirBuilder::AddTrigger(lir::Trigger trigger) {
  if (!current_process_) {
    throw std::runtime_error("AddTrigger called with no active process");
  }

  current_process_->trigger_list.push_back(std::move(trigger));
}

void LirBuilder::StartBlock(const std::string& label) {
  if (!current_process_) {
    throw std::runtime_error("StartBlock called with no active process");
  }

  // If we have an active block, end it before starting a new one
  if (current_block_) {
    EndBlock();
  }

  current_block_ = std::make_unique<lir::BasicBlock>();
  current_block_->label = label;
}

void LirBuilder::EndBlock() {
  if (!current_process_) {
    throw std::runtime_error("EndBlock called with no active process");
  }

  if (!current_block_) {
    throw std::runtime_error("EndBlock called with no active block");
  }

  current_blocks_.push_back(std::move(current_block_));
  current_block_ = nullptr;
}

auto LirBuilder::NewLabel(const std::string& prefix) -> std::string {
  return prefix + "_" + std::to_string(label_counter_++);
}

void LirBuilder::AddInstruction(
    lir::InstructionKind kind, const std::string& result,
    std::vector<lir::Value> operands) {
  if (!current_process_) {
    throw std::runtime_error("AddInstruction called with no active process");
  }

  lir::Instruction instr;
  instr.kind = kind;
  instr.result = result;
  instr.operands = std::move(operands);

  // If we have an active basic block, add to it
  if (current_block_) {
    current_block_->instructions.push_back(std::move(instr));
  }
  // Otherwise, fall back to the legacy flat instruction list
  else {
    current_process_->instructions.push_back(std::move(instr));
  }
}

void LirBuilder::AddInstruction(
    lir::InstructionKind kind, const std::string& result,
    std::vector<lir::Value> operands, const std::string& system_call_name) {
  if (!current_process_) {
    throw std::runtime_error("AddInstruction called with no active process");
  }

  lir::Instruction instr;
  instr.kind = kind;
  instr.result = result;
  instr.operands = std::move(operands);
  instr.system_call_name = system_call_name;

  // If we have an active basic block, add to it
  if (current_block_) {
    current_block_->instructions.push_back(std::move(instr));
  }
  // Otherwise, fall back to the legacy flat instruction list
  else {
    current_process_->instructions.push_back(std::move(instr));
  }
}

void LirBuilder::EndProcess() {
  if (!current_process_) {
    throw std::runtime_error("EndProcess called with no active process");
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
  mod->signals = std::move(signals_);
  mod->processes = std::move(processes_);
  return mod;
}

}  // namespace lyra::lowering
