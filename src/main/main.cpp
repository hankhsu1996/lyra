#include <iostream>
#include <memory>
#include <string>

#include "core/execution_context.hpp"
#include "frontend/slang_frontend.hpp"
#include "lir/executor.hpp"
#include "lowering/ast_to_mir.hpp"
#include "lowering/mir_to_lir.hpp"

using volans::ExecutionContext;
using volans::frontend::LoadCompilation;
using volans::lir::Executor;
using volans::lowering::AstToMir;
using volans::lowering::MirToLir;

auto main() -> int {
  std::cout << "===== Volans Simulator Prototype =====\n";

  const std::string test_file_path = "src/main/test.sv";
  std::cout << "Parsing file: " << test_file_path << "\n";

  auto compilation = LoadCompilation({test_file_path});
  if (!compilation) {
    std::cerr << "Failed to parse file.\n";
    return 1;
  }

  auto mir = AstToMir(*compilation);
  auto lir = MirToLir(mir);

  std::cout << "\n--- LIR Dump ---\n";
  std::cout << "Module: " << lir.name << "\n";

  std::cout << "Signals:\n";
  for (const auto& sig : lir.signals) {
    std::cout << "  " << sig << "\n";
  }

  std::cout << "Processes:\n";
  for (const auto& proc : lir.processes) {
    std::cout << "  Process kind: ";
    switch (proc->kind) {
      case volans::lir::ProcessKind::kInitial:
        std::cout << "initial\n";
        break;
      case volans::lir::ProcessKind::kAlwaysComb:
        std::cout << "always_comb\n";
        break;
      case volans::lir::ProcessKind::kAlwaysFF:
        std::cout << "always_ff\n";
        break;
    }

    for (const auto& instr : proc->instructions) {
      std::cout << "    ";
      switch (instr.kind) {
        case volans::lir::InstructionKind::kLiteralInt:
        case volans::lir::InstructionKind::kLiteralString:
        case volans::lir::InstructionKind::kLoadSignal:
        case volans::lir::InstructionKind::kBinaryAdd:
          std::cout << instr.result << " = ";
          if (instr.kind == volans::lir::InstructionKind::kBinaryAdd) {
            std::cout << "add " << instr.operands[0].ToString() << ", "
                      << instr.operands[1].ToString() << "\n";
          } else if (!instr.operands.empty()) {
            std::cout << instr.operands[0].ToString() << "\n";
          } else {
            std::cout << "(missing operand)\n";
          }
          break;

        case volans::lir::InstructionKind::kAssign:
          if (instr.operands.size() == 2) {
            std::cout << instr.operands[0].ToString() << " = "
                      << instr.operands[1].ToString() << "\n";
          } else {
            std::cout << "(invalid assign operands)\n";
          }
          break;

        case volans::lir::InstructionKind::kStoreSignal:
          if (instr.operands.size() >= 2) {
            std::cout << "store " << instr.operands[0].ToString() << " ← "
                      << instr.operands[1].ToString() << "\n";
          } else {
            std::cout << "store (invalid operands)\n";
          }
          break;

        default:
          std::cout << "(unhandled instruction)\n";
      }
    }
  }

  // ===============================
  // 🧪 Run interpreter on initial block
  // ===============================
  std::cout << "\n--- Simulation Result ---\n";

  ExecutionContext ctx;
  Executor executor(lir, ctx);
  executor.RunInitial();

  for (const auto& sig : lir.signals) {
    try {
      const auto& val = ctx.signalTable.Read(sig);
      std::cout << sig << " = ";
      try {
        std::cout << val.AsInt() << "\n";
      } catch (...) {
        std::cout << val.AsString() << "\n";
      }
    } catch (...) {
      std::cout << sig << " (uninitialized)\n";
    }
  }

  return 0;
}
