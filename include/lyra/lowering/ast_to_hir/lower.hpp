#pragma once

#include <cstdint>
#include <memory>

#include <slang/ast/Compilation.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/lowering/ast_to_hir/options.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/ast_to_hir/source_mapper.hpp"
#include "lyra/mir/instance.hpp"

namespace lyra::lowering::ast_to_hir {

struct LoweringResult {
  hir::Design design;
  // Design-global HIR arena for design-level HIR nodes (package lowering,
  // port binding expressions). Body-local HIR is in each ModuleBody's arena.
  std::unique_ptr<hir::Arena> hir_arena;
  std::unique_ptr<TypeArena> type_arena;
  // Design-global constant arena for Phase 0 constants (parameter defaults,
  // package constants). Body-local constants are in each ModuleBody's
  // constant_arena. This arena may be empty today but represents the
  // design-global constant domain.
  std::unique_ptr<ConstantArena> constant_arena;
  std::unique_ptr<SymbolTable> symbol_table;
  std::unique_ptr<ScopeTable> scope_table;
  std::unique_ptr<SourceManager> source_manager;
  std::unique_ptr<SourceMapper> source_mapper;
  DesignBindingPlan binding_plan;
  common::SpecializationMap specialization_map;
  mir::InstanceTable instance_table;  // For %m support
  int8_t global_precision_power =
      -9;  // Finest timeprecision across all modules
};

auto LowerAstToHir(
    slang::ast::Compilation& compilation, DiagnosticSink& sink,
    const HirLoweringOptions& options) -> LoweringResult;

}  // namespace lyra::lowering::ast_to_hir
