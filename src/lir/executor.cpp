#include "lir/executor.hpp"

#include <fmt/core.h>

namespace lyra::lir {

Executor::Executor(const Module& module, lyra::ExecutionContext& context)
    : module_(module), ctx_(context) {
}

void Executor::RunInitial() {
  for (const auto& process : module_.get().processes) {
    if (process->kind != ProcessKind::kInitial) {
      continue;
    }

    for (const auto& instr : process->instructions) {
      switch (instr.kind) {
        case InstructionKind::kLiteralInt: {
          int val = std::get<int>(instr.operands[0].data);
          ctx_.get().WriteSSA(instr.result, RuntimeValue::FromInt(val));
          break;
        }

        case InstructionKind::kLiteralString: {
          const auto& val = std::get<std::string>(instr.operands[0].data);
          ctx_.get().WriteSSA(instr.result, RuntimeValue::FromString(val));
          break;
        }

        case InstructionKind::kBinaryAdd: {
          auto lhs = std::get<std::string>(instr.operands[0].data);
          auto rhs = std::get<std::string>(instr.operands[1].data);
          int v1 = ctx_.get().ReadSSA(lhs).AsInt();
          int v2 = ctx_.get().ReadSSA(rhs).AsInt();
          ctx_.get().WriteSSA(instr.result, RuntimeValue::FromInt(v1 + v2));
          break;
        }

        case InstructionKind::kLoadSignal: {
          auto src_signal = std::get<std::string>(instr.operands[0].data);
          RuntimeValue val = ctx_.get().signalTable.Read(src_signal);
          ctx_.get().WriteSSA(instr.result, val);
          break;
        }

        case InstructionKind::kStoreSignal: {
          auto dst_signal = std::get<std::string>(instr.operands[0].data);
          auto src_val = std::get<std::string>(instr.operands[1].data);
          ctx_.get().signalTable.Write(dst_signal, ctx_.get().ReadSSA(src_val));
          break;
        }

        default:
          throw std::runtime_error(fmt::format(
              "Unhandled instruction {} in executor", instr.ToString()));
      }
    }
  }
}

}  // namespace lyra::lir
