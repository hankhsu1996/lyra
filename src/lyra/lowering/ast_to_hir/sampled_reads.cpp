#include "lyra/lowering/ast_to_hir/sampled_reads.hpp"

#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Whether a variable answers for a sampled value with its current value, which
// an automatic variable does (LRM 16.5.1) -- and so does a `ref` formal that is
// not `ref static`, being usable only where an automatic variable is (LRM
// 13.5.2). A property of an object has a lifetime too, but it is the object's
// rather than a call's, and the clause does not reach it.
auto SampledAsCurrent(const slang::ast::ValueSymbol& symbol) -> bool {
  const auto* var = symbol.as_if<slang::ast::VariableSymbol>();
  return var != nullptr &&
         symbol.kind != slang::ast::SymbolKind::ClassProperty &&
         var->lifetime == slang::ast::VariableLifetime::Automatic &&
         !var->flags.has(slang::ast::VariableFlags::RefStatic);
}

}  // namespace

auto ArmSampledReads(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<SampledReads> {
  if (frame.reader_scope == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value read outside a design element's scope is not yet "
        "supported");
  }
  const auto& reads = unit_lowerer.Sensitivity().AnalyzeReads(
      expr, frame.reader_scope->asSymbol());
  std::vector<AccessedPart> armed;
  SampledReads counted;
  for (const AccessedPart& read : reads) {
    const auto* var = read.symbol->as_if<slang::ast::VariableSymbol>();
    if (var != nullptr &&
        var->flags.has(slang::ast::VariableFlags::RefStatic)) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a sampled value of a 'ref static' argument is not yet supported");
    }
    if (SampledAsCurrent(*read.symbol)) {
      ++counted.current;
      continue;
    }
    armed.push_back(read);
  }
  counted.armed = armed.size();
  if (armed.empty()) return counted;
  if (frame.current_structural_scope == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value read outside a design element's scope is not yet "
        "supported");
  }
  auto cells = unit_lowerer.CellsRead(armed, frame);
  if (!cells) return std::unexpected(std::move(cells.error()));
  // Every variable the expression reads has to be armed, so a read the
  // translation could not name as a cell of this design leaves one that can
  // never answer. The reachable case is a property of an object (LRM 8.4),
  // which no scope holds a cell of.
  if (cells->size() < armed.size()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value of storage this design element does not hold is "
        "not yet supported");
  }
  std::vector<hir::ValueTarget>& sampled =
      frame.current_structural_scope->sampled_cells;
  for (hir::ValueTarget& cell : *cells) {
    sampled.push_back(std::move(cell));
  }
  return counted;
}

}  // namespace lyra::lowering::ast_to_hir
