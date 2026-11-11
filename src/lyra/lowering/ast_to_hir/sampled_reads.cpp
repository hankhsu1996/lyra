#include "lyra/lowering/ast_to_hir/sampled_reads.hpp"

#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Whether one read answers for its part of an expression's sampled value
// (LRM 16.5.1) from a cell the reading scope arms. Three kinds of read answer
// without one: an automatic variable with what it holds now, as a `ref` formal
// that is not `ref static` does, being usable only where an automatic is (LRM
// 13.5.2); a constant, which is its own value at every time; and a clocking
// block input, whose sampled value is the one its block samples (LRM 14.13),
// computed from what the block itself arms. Anything else does, a constant
// read from a cell among them.
auto AnswersFromArmedCell(
    const UnitLowerer& unit_lowerer, const slang::ast::Scope& reader,
    const slang::ast::ValueSymbol& symbol, diag::SourceSpan span)
    -> diag::Result<bool> {
  if (symbol.kind == slang::ast::SymbolKind::ClockVar) {
    // The sample is computed where the block is declared, so a read through
    // the hierarchy would have to reach the block's signal from there.
    const slang::ast::Scope* block = symbol.getParentScope();
    if (block == nullptr || block->asSymbol().getParentScope() != &reader) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a sampled value of an input of a clocking block another scope "
          "declares is not yet supported");
    }
    return false;
  }
  const auto* var = symbol.as_if<slang::ast::VariableSymbol>();
  // A property of an object has a lifetime too, but it is the object's rather
  // than a call's, and the clause does not reach it.
  if (var != nullptr && symbol.kind != slang::ast::SymbolKind::ClassProperty) {
    if (var->flags.has(slang::ast::VariableFlags::RefStatic)) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a sampled value of a 'ref static' argument is not yet supported");
    }
    if (!HasStaticStorage(*var)) {
      return false;
    }
  }
  auto resolved = ResolveReferent(symbol, span);
  if (!resolved) return std::unexpected(std::move(resolved.error()));
  switch (resolved->kind) {
    case Referent::kParameterConstant:
      return unit_lowerer.ReadsParameterFromCell(
          resolved->symbol->as<slang::ast::ParameterSymbol>());
    case Referent::kEnumConstant:
    case Referent::kSpecparam:
      return false;
    case Referent::kVariableStorage:
    case Referent::kNetStorage:
    // What names no cell this scope can reach is still a read that has to
    // answer, so it goes to arming and is refused there.
    case Referent::kClassProperty:
    case Referent::kThisHandle:
    case Referent::kPatternBinding:
    case Referent::kViewDefinedName:
    case Referent::kPrimitivePort:
    case Referent::kClockingOutput:
    case Referent::kAssertionLocal:
    case Referent::kStructureMember:
      return true;
    case Referent::kNotAValue:
      break;
  }
  throw InternalError(
      "AnswersFromArmedCell: a read resolved to a declaration that denotes no "
      "value");
}

}  // namespace

auto ArmSampledReads(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<void> {
  if (frame.reader_scope == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value read outside a design element's scope is not yet "
        "supported");
  }
  const auto& reads = unit_lowerer.Sensitivity().AnalyzeReads(
      expr, frame.reader_scope->asSymbol());
  std::vector<AccessedPart> armed;
  for (const AccessedPart& read : reads) {
    auto answers = AnswersFromArmedCell(
        unit_lowerer, *frame.reader_scope, *read.symbol, span);
    if (!answers) return std::unexpected(std::move(answers.error()));
    if (*answers) armed.push_back(read);
  }
  if (armed.empty()) return {};
  if (frame.current_structural_scope == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value read outside a design element's scope is not yet "
        "supported");
  }
  auto cells = unit_lowerer.CellsRead(armed, frame);
  if (!cells) return std::unexpected(std::move(cells.error()));
  // Every read left here has to answer from a cell, so a read the translation
  // could not name as a cell of this design leaves one that can never answer.
  // The reachable case is a property of an object (LRM 8.4), which no scope
  // holds a cell of.
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
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
