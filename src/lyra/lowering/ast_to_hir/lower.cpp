#include "lyra/lowering/ast_to_hir/lower.hpp"

#include <cstdint>
#include <memory>
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

auto LowerAstToHir(
    slang::ast::Compilation& compilation, DiagnosticSink& sink,
    const HirLoweringOptions& options) -> AstToHirOutput {
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

  Context ctx;
  ctx.options = &options;
  ctx.sink = &sink;
  ctx.hir_arena = hir_arena.get();
  ctx.type_arena = type_arena.get();
  ctx.active_constant_arena = constant_arena.get();
  ctx.symbol_table = symbol_table.get();
  ctx.scope_table = scope_table.get();
  ctx.source_mapper = source_mapper.get();

  SymbolRegistrar registrar(&ctx);

  // Callable signature table accumulates during lowering, then moves into
  // the returned Design as persistent semantic state.
  hir::HirCallableSignatureTable callable_signatures;
  ctx.callable_signatures = &callable_signatures;

  DesignLoweringOutput design_output;
  {
    ScopeGuard scope_guard(registrar, ScopeKind::kRoot);
    design_output = LowerDesign(compilation, registrar, &ctx);
  }

  design_output.hir.design.callable_signatures = std::move(callable_signatures);
  ctx.callable_signatures = nullptr;

  auto global_precision =
      static_cast<int8_t>(ComputeGlobalPrecision(compilation));

  LoweringResult hir_result{
      .design = std::move(design_output.hir.design),
      .hir_arena = std::move(hir_arena),
      .type_arena = std::move(type_arena),
      .constant_arena = std::move(constant_arena),
      .symbol_table = std::move(symbol_table),
      .scope_table = std::move(scope_table),
      .source_manager = std::move(source_manager),
      .source_mapper = std::move(source_mapper),
  };

  CompositionMetadata composition{
      .binding_plan = std::move(design_output.composition.binding_plan),
      .specialization_map =
          std::move(design_output.composition.specialization_map),
      .instance_table = std::move(design_output.composition.instance_table),
      .global_precision_power = global_precision,
      .body_timescales = std::move(design_output.composition.body_timescales),
      .child_coord_map = std::move(design_output.composition.child_coord_map),
      .hierarchy_nodes = std::move(design_output.composition.hierarchy_nodes),
      .dpi_export_signatures =
          std::move(design_output.composition.dpi_export_signatures),
  };

  return AstToHirOutput{
      .hir = std::move(hir_result),
      .composition = std::move(composition),
  };
}

}  // namespace lyra::lowering::ast_to_hir
