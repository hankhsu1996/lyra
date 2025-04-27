#include <iostream>
#include <memory>
#include <string>

#include <slang/ast/Compilation.h>
#include <spdlog/spdlog.h>

#include "core/execution_context.hpp"
#include "frontend/slang_frontend.hpp"
#include "lir/executor.hpp"
#include "lowering/ast_to_mir/ast_to_mir.hpp"
#include "lowering/mir_to_lir/mir_to_lir.hpp"

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

  // Dump LIR
  std::cout << "\n--- LIR Dump ---\n";
  std::cout << lir << "\n";

  // Run interpreter
  std::cout << "\n--- Simulation Result ---\n";

  ExecutionContext ctx;
  Executor executor(lir, ctx);
  executor.RunInitial();

  for (const auto& sig : lir.signals) {
    const auto& val = ctx.signalTable.Read(sig);
    std::cout << sig << " = " << val.AsInt() << "\n";
  }

  return 0;
}
