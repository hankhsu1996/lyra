#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto AddChildStructuralScope(hir::Generate& gen, hir::StructuralScope scope)
    -> hir::StructuralScopeId {
  const hir::StructuralScopeId id{
      static_cast<std::uint32_t>(gen.child_scopes.size())};
  scope.id = id;
  gen.child_scopes.Add(std::move(scope));
  return id;
}

// Lowers one generate block -- a loop iteration, an `if` / `case` arm, or a
// bare block -- into a fresh concrete structural scope. No runtime loop
// variable is bound, so a genvar reference in the body folds to this block's
// concrete value.
auto LowerGenerateScope(
    UnitLowerer& unit_lowerer, const slang::ast::GenerateBlockSymbol& block,
    std::string_view source_name, WalkFrame frame)
    -> diag::Result<hir::StructuralScope> {
  StructuralScopeLowerer child(unit_lowerer, block);
  auto scope_or = child.Run(frame);
  if (!scope_or) return std::unexpected(std::move(scope_or.error()));
  scope_or->source_name = std::string{source_name};
  return scope_or;
}

}  // namespace

auto StructuralScopeLowerer::BuildResolvedGenerateFromArray(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  hir::Generate gen{};
  std::vector<hir::ResolvedGenerateItem> items;
  items.reserve(array.entries.size());
  for (const auto* entry : array.entries) {
    auto scope_or = LowerGenerateScope(*owner_, *entry, array.name, frame);
    if (!scope_or) return std::unexpected(std::move(scope_or.error()));
    const hir::StructuralScopeId scope_id =
        AddChildStructuralScope(gen, *std::move(scope_or));

    const slang::SVInt* array_index = entry->getArrayIndex();
    if (array_index == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::BuildResolvedGenerateFromArray: loop "
          "iteration entry carries no array index");
    }
    items.push_back(
        hir::ResolvedGenerateItem{
            .index = array_index->as<std::int64_t>().value_or(0),
            .scope = scope_id});
  }
  gen.data = hir::ResolvedGenerate{.items = std::move(items)};
  return gen;
}

auto StructuralScopeLowerer::BuildResolvedGenerateFromBlock(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  hir::Generate gen{};
  auto scope_or = LowerGenerateScope(*owner_, block, block.name, frame);
  if (!scope_or) return std::unexpected(std::move(scope_or.error()));
  const hir::StructuralScopeId scope_id =
      AddChildStructuralScope(gen, *std::move(scope_or));
  gen.data = hir::ResolvedGenerate{
      .items = {
          hir::ResolvedGenerateItem{.index = std::nullopt, .scope = scope_id}}};
  return gen;
}

}  // namespace lyra::lowering::ast_to_hir
