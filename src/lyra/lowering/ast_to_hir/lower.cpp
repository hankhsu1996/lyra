#include "lyra/lowering/ast_to_hir/lower.hpp"

#include "lyra/common/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/design.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

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
  };

  SymbolRegistrar registrar(&ctx);

  // Establish root scope before lowering
  registrar.PushScope(ScopeKind::kRoot);

  hir::Design design = LowerDesign(compilation, registrar, &ctx);

  registrar.PopScope();

  return LoweringResult{
      .design = std::move(design),
      .hir_arena = std::move(hir_arena),
      .type_arena = std::move(type_arena),
      .constant_arena = std::move(constant_arena),
      .symbol_table = std::move(symbol_table),
      .scope_table = std::move(scope_table),
      .source_manager = std::move(source_manager),
      .source_mapper = std::move(source_mapper),
  };
}

}  // namespace lyra::lowering::ast_to_hir
