#include "dump.hpp"

#include <iostream>

#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/hir/dumper.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/dumper.hpp"
#include "print.hpp"

namespace lyra::driver {

auto DumpHir(const CompilationInput& input) -> int {
  auto parse_result = LoadFiles(input);
  if (!parse_result) {
    return 1;
  }

  DiagnosticSink sink;
  auto result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result->compilation, sink);

  if (sink.HasErrors()) {
    PrintDiagnostics(sink, result.source_manager.get());
    return 1;
  }

  hir::Dumper dumper(
      result.hir_arena.get(), result.type_arena.get(),
      result.constant_arena.get(), result.symbol_table.get(), &std::cout);
  dumper.Dump(result.design);

  return 0;
}

auto DumpMir(const CompilationInput& input) -> int {
  auto parse_result = LoadFiles(input);
  if (!parse_result) {
    return 1;
  }

  DiagnosticSink sink;
  auto hir_result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result->compilation, sink);

  if (sink.HasErrors()) {
    PrintDiagnostics(sink, hir_result.source_manager.get());
    return 1;
  }

  lowering::hir_to_mir::LoweringInput mir_input{
      .design = &hir_result.design,
      .hir_arena = hir_result.hir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .constant_arena = hir_result.constant_arena.get(),
      .symbol_table = hir_result.symbol_table.get(),
      .builtin_types = {},
  };
  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);

  mir::Dumper dumper(
      mir_result.mir_arena.get(), hir_result.type_arena.get(), &std::cout);
  dumper.Dump(mir_result.design);

  return 0;
}

auto DumpLlvm(const CompilationInput& input) -> int {
  auto parse_result = LoadFiles(input);
  if (!parse_result) {
    return 1;
  }

  DiagnosticSink sink;
  auto hir_result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result->compilation, sink);

  if (sink.HasErrors()) {
    PrintDiagnostics(sink, hir_result.source_manager.get());
    return 1;
  }

  lowering::hir_to_mir::LoweringInput mir_input{
      .design = &hir_result.design,
      .hir_arena = hir_result.hir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .constant_arena = hir_result.constant_arena.get(),
      .symbol_table = hir_result.symbol_table.get(),
      .builtin_types = {},
  };
  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &mir_result.design,
      .mir_arena = mir_result.mir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .variables = {},
  };
  auto llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);

  std::cout << lowering::mir_to_llvm::DumpLlvmIr(llvm_result);

  return 0;
}

}  // namespace lyra::driver
