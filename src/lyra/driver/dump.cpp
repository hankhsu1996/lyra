#include "dump.hpp"

#include <iostream>
#include <vector>

#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/dumper.hpp"
#include "lyra/hir/module.hpp"
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

  // Build slot_types and slot_type_ids from HIR for proper initialization
  std::vector<lowering::mir_to_llvm::SlotTypeInfo> slot_types;
  std::vector<TypeId> slot_type_ids;
  for (const auto& element : hir_result.design.elements) {
    if (const auto* hir_module = std::get_if<hir::Module>(&element)) {
      const auto& type_arena = *hir_result.type_arena;
      const auto& symbol_table = *hir_result.symbol_table;
      for (SymbolId sym_id : hir_module->variables) {
        const auto& sym = symbol_table[sym_id];
        const Type& type = type_arena[sym.type];
        slot_type_ids.push_back(sym.type);
        if (type.Kind() == TypeKind::kReal) {
          slot_types.push_back({
              .kind = lowering::mir_to_llvm::VarTypeKind::kReal,
              .width = 64,
              .is_signed = true,
          });
        } else if (type.Kind() == TypeKind::kString) {
          slot_types.push_back({
              .kind = lowering::mir_to_llvm::VarTypeKind::kString,
              .width = 0,
              .is_signed = false,
          });
        } else if (IsPacked(type)) {
          uint32_t width = PackedBitWidth(type, type_arena);
          slot_types.push_back({
              .kind = lowering::mir_to_llvm::VarTypeKind::kIntegral,
              .width = width > 0 ? width : 32,
              .is_signed = IsPackedSigned(type, type_arena),
          });
        } else {
          slot_types.push_back({
              .kind = lowering::mir_to_llvm::VarTypeKind::kIntegral,
              .width = 32,
              .is_signed = false,
          });
        }
      }
      break;  // Single module only
    }
  }

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &mir_result.design,
      .mir_arena = mir_result.mir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .slot_types = std::move(slot_types),
      .slot_type_ids = std::move(slot_type_ids),
      .variables = {},
  };
  auto llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);

  std::cout << lowering::mir_to_llvm::DumpLlvmIr(llvm_result);

  return 0;
}

}  // namespace lyra::driver
