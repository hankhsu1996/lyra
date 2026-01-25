#pragma once

#include <memory>

#include <slang/ast/Compilation.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/ast_to_hir/source_mapper.hpp"

namespace lyra::lowering::ast_to_hir {

struct LoweringResult {
  hir::Design design;
  std::unique_ptr<hir::Arena> hir_arena;
  std::unique_ptr<TypeArena> type_arena;
  std::unique_ptr<ConstantArena> constant_arena;
  std::unique_ptr<SymbolTable> symbol_table;
  std::unique_ptr<ScopeTable> scope_table;
  std::unique_ptr<SourceManager> source_manager;
  std::unique_ptr<SourceMapper> source_mapper;
  DesignBindingPlan binding_plan;
};

auto LowerAstToHir(slang::ast::Compilation& compilation, DiagnosticSink& sink)
    -> LoweringResult;

}  // namespace lyra::lowering::ast_to_hir
