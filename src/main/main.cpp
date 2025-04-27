#include <iostream>
#include <memory>
#include <string>

#include <slang/ast/Compilation.h>
#include <spdlog/spdlog.h>

#include "core/execution_context.hpp"
#include "frontend/slang_frontend.hpp"
#include "lir/executor.hpp"
#include "lowering/ast_to_mir/ast_to_mir.hpp"
#include "lowering/mir_to_lir.hpp"

using lyra::ExecutionContext;
using lyra::frontend::LoadCompilation;
using lyra::lir::Executor;
using lyra::lowering::AstToMir;
using lyra::lowering::MirToLir;

auto main() -> int {
  spdlog::set_level(spdlog::level::debug);
  spdlog::flush_on(spdlog::level::debug);

  std::cout << "===== Lyra Simulator Prototype =====\n";

  const std::string test_file_path = "src/main/test.sv";
  std::cout << "Parsing file: " << test_file_path << "\n";

  auto compilation = LoadCompilation({test_file_path});
  if (!compilation) {
    std::cerr << "Failed to parse file.\n";
    return 1;
  }

  // Convert AST to MIR
  auto mir = AstToMir(*compilation);

  // Convert MIR to LIR
  auto lir = MirToLir(*mir);

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
      case lyra::lir::ProcessKind::kInitial:
        std::cout << "initial\n";
        break;
      case lyra::lir::ProcessKind::kAlwaysComb:
        std::cout << "always_comb\n";
        break;
      case lyra::lir::ProcessKind::kAlwaysFF:
        std::cout << "always_ff\n";
        break;
    }

    for (const auto& instr : proc->instructions) {
      std::cout << "    ";
      switch (instr.kind) {
        case lyra::lir::InstructionKind::kLiteralInt:
        case lyra::lir::InstructionKind::kLiteralString:
        case lyra::lir::InstructionKind::kLoadSignal:
        case lyra::lir::InstructionKind::kBinaryAdd:
          std::cout << instr.result << " = ";
          if (instr.kind == lyra::lir::InstructionKind::kBinaryAdd) {
            std::cout << "add " << instr.operands[0].ToString() << ", "
                      << instr.operands[1].ToString() << "\n";
          } else if (!instr.operands.empty()) {
            std::cout << instr.operands[0].ToString() << "\n";
          } else {
            std::cout << "(missing operand)\n";
          }
          break;

        case lyra::lir::InstructionKind::kAssign:
          if (instr.operands.size() == 2) {
            std::cout << instr.operands[0].ToString() << " = "
                      << instr.operands[1].ToString() << "\n";
          } else {
            std::cout << "(invalid assign operands)\n";
          }
          break;

        case lyra::lir::InstructionKind::kStoreSignal:
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

  // Run interpreter
  std::cout << "\n--- Simulation Result ---\n";

  ExecutionContext ctx;
  Executor executor(lir, ctx);
  executor.RunInitial();

  for (const auto& sig : lir.signals) {
    const auto& val = ctx.signalTable.Read(sig);
    std::cout << sig << " = ";
    std::cout << val.AsInt() << "\n";
  }

  return 0;
}
