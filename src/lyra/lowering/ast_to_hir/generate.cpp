#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Lowers one generate block -- a loop iteration, an `if` / `case` arm, or a
// bare block -- into a fresh concrete structural scope. No runtime loop
// variable is bound, so a genvar reference in the body folds to this block's
// concrete value. The source label and the index are the two halves of the
// block's hierarchy segment, so both are settled here, on the scope itself.
auto LowerGenerateScope(
    UnitLowerer& unit_lowerer, const slang::ast::GenerateBlockSymbol& block,
    std::string_view source_name, std::optional<std::int64_t> index,
    WalkFrame frame) -> diag::Result<hir::StructuralScope> {
  StructuralScopeLowerer child(unit_lowerer, block);
  auto scope_or = child.Run(frame);
  if (!scope_or) return std::unexpected(std::move(scope_or.error()));
  scope_or->source_name = std::string{source_name};
  scope_or->index = index;
  return scope_or;
}

}  // namespace

auto StructuralScopeLowerer::BuildGenerateFromArray(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  hir::Generate gen{};
  for (const auto* entry : array.entries) {
    const slang::SVInt* array_index = entry->getArrayIndex();
    if (array_index == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::BuildGenerateFromArray: loop iteration "
          "entry carries no array index");
    }
    auto scope_or = LowerGenerateScope(
        *owner_, *entry, array.name,
        array_index->as<std::int64_t>().value_or(0), frame);
    if (!scope_or) return std::unexpected(std::move(scope_or.error()));
    gen.child_scopes.Add(*std::move(scope_or));
  }
  return gen;
}

auto StructuralScopeLowerer::BuildGenerateFromBlock(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  hir::Generate gen{};
  auto scope_or =
      LowerGenerateScope(*owner_, block, block.name, std::nullopt, frame);
  if (!scope_or) return std::unexpected(std::move(scope_or.error()));
  gen.child_scopes.Add(*std::move(scope_or));
  return gen;
}

}  // namespace lyra::lowering::ast_to_hir
