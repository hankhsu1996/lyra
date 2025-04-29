#include "lir/executor.hpp"

#include <fmt/core.h>

namespace lyra::lir {

Executor::Executor(const Module& module, lyra::ExecutionContext& context)
    : module_(module), ctx_(context) {
}

// Execute a single instruction in the given context
auto Executor::ExecuteInstruction(const Instruction& instr) -> ExecuteResult {
  switch (instr.kind) {
    case InstructionKind::kLiteralInt: {
      int64_t val = std::get<int64_t>(instr.operands[0].data);
      ctx_.get().ssaTable.Write(instr.result, RuntimeValue::FromInt(val));
      return ExecuteResult::Continue();
    }

    case InstructionKind::kLiteralString: {
      const auto& val = std::get<std::string>(instr.operands[0].data);
      ctx_.get().ssaTable.Write(instr.result, RuntimeValue::FromString(val));
      return ExecuteResult::Continue();
    }

    case InstructionKind::kBinaryAdd: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      int64_t v1 = ctx_.get().ssaTable.Read(lhs).AsInt();
      int64_t v2 = ctx_.get().ssaTable.Read(rhs).AsInt();
      ctx_.get().ssaTable.Write(instr.result, RuntimeValue::FromInt(v1 + v2));
      return ExecuteResult::Continue();
    }

    case InstructionKind::kLoadSignal: {
      auto src_signal = std::get<std::string>(instr.operands[0].data);
      RuntimeValue val = ctx_.get().signalTable.Read(src_signal);
      ctx_.get().ssaTable.Write(instr.result, val);
      return ExecuteResult::Continue();
    }

    case InstructionKind::kStoreSignal: {
      auto dst_signal = std::get<std::string>(instr.operands[0].data);
      auto src_val = std::get<std::string>(instr.operands[1].data);
      ctx_.get().signalTable.Write(
          dst_signal, ctx_.get().ssaTable.Read(src_val));
      return ExecuteResult::Continue();
    }

    case InstructionKind::kDelay: {
      auto delay_amount = std::get<int64_t>(instr.operands[0].data);
      return ExecuteResult::Delay(delay_amount);
    }

    default:
      throw std::runtime_error(fmt::format(
          "Unhandled instruction {} in executor", instr.ToString()));
  }
}

}  // namespace lyra::lir
