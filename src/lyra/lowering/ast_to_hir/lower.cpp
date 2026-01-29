#include "lyra/lowering/ast_to_hir/lower.hpp"

#include <cstdint>
#include <memory>
#include <optional>
#include <utility>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/scope_table.hpp"
#include "lyra/common/scope_types.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/common/symbol_table.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/design.hpp"
#include "lyra/lowering/ast_to_hir/source_mapper.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/timescale.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerAstToHir(slang::ast::Compilation& compilation, DiagnosticSink& sink)
    -> LoweringResult {
  if (compilation.getSourceManager() == nullptr) {
    throw common::InternalError(
        "AST to HIR lowering", "compilation has no source manager");
  }

  auto hir_arena = std::make_unique<hir::Arena>();
  auto type_arena = std::make_unique<TypeArena>();
  auto constant_arena = std::make_unique<ConstantArena>();
  auto symbol_table = std::make_unique<SymbolTable>();
  auto scope_table = std::make_unique<ScopeTable>();
  auto source_manager = std::make_unique<SourceManager>();
  auto source_mapper = std::make_unique<SourceMapper>();

  RegisterSourceFiles(
      *compilation.getSourceManager(), *source_manager, *source_mapper);

  Context ctx{
      .sink = &sink,
      .hir_arena = hir_arena.get(),
      .type_arena = type_arena.get(),
      .constant_arena = constant_arena.get(),
      .symbol_table = symbol_table.get(),
      .scope_table = scope_table.get(),
      .source_mapper = source_mapper.get(),
      .temp_counter = 0,
      .cached_global_precision = std::nullopt,
  };

  SymbolRegistrar registrar(&ctx);

  DesignLoweringResult design_result;
  {
    ScopeGuard scope_guard(registrar, ScopeKind::kRoot);
    design_result = LowerDesign(compilation, registrar, &ctx);
  }

  auto global_precision =
      static_cast<int8_t>(ComputeGlobalPrecision(compilation));

  return LoweringResult{
      .design = std::move(design_result.design),
      .hir_arena = std::move(hir_arena),
      .type_arena = std::move(type_arena),
      .constant_arena = std::move(constant_arena),
      .symbol_table = std::move(symbol_table),
      .scope_table = std::move(scope_table),
      .source_manager = std::move(source_manager),
      .source_mapper = std::move(source_mapper),
      .binding_plan = std::move(design_result.binding_plan),
      .instance_table = std::move(design_result.instance_table),
      .global_precision_power = global_precision,
  };
}

}  // namespace lyra::lowering::ast_to_hir
