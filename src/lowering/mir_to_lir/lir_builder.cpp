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

  current_process_->instructions.push_back(std::move(instr));
}

void LirBuilder::EndProcess() {
  if (!current_process_) {
    throw std::runtime_error("EndProcess called with no active process");
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

auto LirBuilder::Build() -> lir::Module {
  lir::Module mod;
  mod.name = module_name_;
  mod.signals = std::move(signals_);
  mod.processes = std::move(processes_);
  return mod;
}

}  // namespace lyra::lowering
